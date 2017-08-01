#!/bin/bash
# see https://www.gwern.net/About#markdown-checker

fgp () { fgrep --color=always "$@"; }
egp () { egrep --color=always "$@"; }

for PAGE in "$@"
do
    if [[ $PAGE == *.page ]]; then

        # warn if not text, perhaps due to bad copy-paste
        file "$PAGE" | fgp -v "text";

        # find bad URLS, unacceptable/unreliable domains, malformed syntax, unmatched apostrophes
        fgp -e "http://dl.dropbox" -e "http://news.ycombinator.com" -e "http://github.com" \
            -e "http://www.coursera.org" -e ".wiley.com/" -e "http://www.ncbi.nlm.nih.gov/pubmed/" \
            -e "www.tandfonline.com/doi/abs/" -e "jstor.org" -e "springer.com" -e "springerlink.com" \
            -e "www.mendeley.com" -e 'academia.edu' -e 'researchgate.net' -e 'pdf.yt' \
            -e 'photobucket' -e 'imgur.com' -- "$PAGE";
        # check for aggregator-hosted PDFs and host them on gwern.net to make them visible to Google Scholar/provide backups:
        link-extractor.hs "$PAGE" | egp --only-matching -e '^http://.*archive\.org/.*\.pdf$';
        egp -e "http://www.pnas.org/content/.*/.*/.*.abstract" -e '[^\.]t\.test\(' -e '^\~\~\{\.' -- "$PAGE";
        fgp -e "<q>" -e "</q>" -e "(www" -e ")www" -e "![](" -e "]()" -e "](/wiki/" -e "](wiki/" \
            -e " percent " -e "    Pearson'" -e '~~~{.sh}' -e 'library("' -- "$PAGE";

        # look for personal uses of illegitimate statistics & weasel words, but filter out blockquotes
        fgp -e ' significant ' -e ' significantly ' -e ' obvious' -e 'basically' -e ' the the ' -- "$PAGE" | egrep -v '[[:space:]]*>';

        # look for English/imperial units as a reminder to switch to metric as much as possible:
        fgp -e " feet" -e " foot " -e ' pound ' -e ' mile' -e inch -- "$PAGE"

        # check for duplicate footnote IDs (force no highlighting, because the terminal escape codes trigger bracket-matching)
        egrep --only-matching '^\[\^.*\]: ' -- "$PAGE" | sort | uniq --count | \
            fgrep --invert-match "      1 [^";

        # image hotlinking deprecated; impolite, and slows page loads & site compiles
        egp --only-matching '\!\[.*\]\(http://.*\)' -- "$PAGE";
        # indicates broken copy-paste of image location
        egp --only-matching '\!\[.*\]\(wiki/.*\)' -- "$PAGE";

        # look for unescaped single dollar-signs (risk of future breakage)
        egp '^[^$]* [^\"]\$[^$]*$' -- "$PAGE";

        # instead of writing 'x = ~y', unicode as '≈'
        fgp -e '= ~' -- "$PAGE" | fgrep -v ' mods'

        [ "$(egrep '^description: ' "$PAGE" | wc --char)" -ge 90 ] || echo "Description metadata too short."

        markdown-length-checker.hs "$PAGE";
        markdown-footnote-length.hs "$PAGE";
        proselint "$PAGE";

        # look for syntax errors making it to the final HTML output:
        HTML=$(mktemp  --suffix=".html")
        tail -n +3 -- "$PAGE" | pandoc --to=html5 --mathml --standalone - --output="$HTML"
        tidy -quiet -errors --doctype html5 "$HTML";
        fgp -e "<""del"">" "$HTML";
        elinks -dump --force-html "$HTML" \
                     | fgp -e '\frac' -e '\times' -e '(http' -e ')http' -e '[http' -e ']http'  \
                           -e ' _ ' -e '[^' -e '^]' -e '/* ' -e ' */' -e '<!--' -e '-->' -e '<-- ' -e '<—' -e '—>' \
                           -e '$title$' -e '<del>' \
                           -e '$description$' -e '$author$' -e '$tags$' -e '$category$' \
                           -e '(!Wikipedia' -e '(!Hoogle' -e 'http://www.gwern.net' -e 'http://gwern.net' ; # ))

        # Finally, check for broken external links; ignore local URLs which are usually either relative links to
        # other pages in the repo or may be interwiki links like '!Wikipedia'.
        linkchecker --no-status --check-extern --threads=1 -r1 --ignore="file://.*" "$HTML"
    fi
    if [[ $PAGE == *.sh ]]; then
        shellcheck "$PAGE"
    fi
    if [[ $PAGE == *.hs ]]; then
        hlint "$PAGE"
    fi
    if [[ $PAGE == *.html ]]; then
        tidy -quiet -errors --doctype html5 "$PAGE"
    fi
done
