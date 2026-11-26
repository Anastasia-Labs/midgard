# Size limits

ESLint caps every TypeScript and JavaScript file under `demo` at 500 lines
(`max-lines` in `demo/eslint.config.mjs`); a file listed in
`demo/module-size-exceptions.json` is capped at its recorded size instead,
and `demo/scripts/check-module-size-exceptions.mjs` holds the native, SQL and
out-of-demo entries to their counts. [eslint: max-lines] Aiken modules have no
line limit. Find the largest files before choosing work:

```bash
git ls-files 'demo/*.ts' 'onchain/aiken/*.ak' | xargs wc -l | grep -v ' total$' \
  | sort -rn | head -20
```
