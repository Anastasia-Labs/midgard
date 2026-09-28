import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  chmodSync,
  copyFileSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

// A bare `aiken fmt` formats the whole Aiken project, which rewrites about 250
// unrelated .ak files and mixes them into whatever work the tree holds. The
// generator must format only the environments it writes. This test copies it
// into a throwaway tree with a fake `aiken` first on PATH and a stub profile
// generator, and reads back what `aiken fmt` was asked to format.

const scriptSource = join(
  dirname(fileURLToPath(import.meta.url)),
  "generate-user-events-witness-script-prefix.sh",
);

const mockRaw = "77".repeat(32);
const prefix = "deadbeef5820";

const fakeAiken = (log) => `#!/usr/bin/env bash
set -euo pipefail
echo "$*" >> '${log}'
out=""
args=("$@")
for ((i = 0; i < \${#args[@]}; i++)); do
  if [ "\${args[i]}" = "--out" ]; then out="\${args[i + 1]}"; fi
done
case "$1" in
  build) echo '{"validators":[]}' > "$out" ;;
  blueprint)
    printf '{"validators":[{"title":"user_events/witness.main.publish","compiledCode":"%s"}]}\\n' \\
      '${prefix}${mockRaw}ffff' > "$out" ;;
  fmt) ;;
  *) exit 42 ;;
esac
`;

// Stands in for `deployment-profiles.mjs generate`: writes two environments.
const stubGenerator = `import { writeFileSync } from "node:fs";
for (const name of ["default", "testnet"]) {
  writeFileSync(new URL(\`../../onchain/aiken/env/\${name}.ak\`, import.meta.url), "pub const x = 1\\n");
}
`;

const withTree = (body) => {
  const scratch = realpathSync(
    mkdtempSync(join(tmpdir(), "witness-prefix-fmt-scope-")),
  );
  try {
    const repo = join(scratch, "repo");
    const bin = join(scratch, "bin");
    const log = join(scratch, "aiken-calls.log");
    const write = (path, contents) => {
      mkdirSync(dirname(join(repo, path)), { recursive: true });
      writeFileSync(join(repo, path), contents);
    };
    write(
      "config/deployments/env.ak.template",
      'pub const user_events_witness_script_prefix: ByteArray =\n  #"00"\n',
    );
    write("demo/scripts/deployment-profiles.mjs", stubGenerator);
    write("onchain/aiken/aiken.toml", 'name = "probe/probe"\n');
    write("onchain/aiken/env/.keep", "");
    write("scripts/.keep", "");
    copyFileSync(
      scriptSource,
      join(repo, "scripts/generate-user-events-witness-script-prefix.sh"),
    );
    mkdirSync(bin);
    writeFileSync(join(bin, "aiken"), fakeAiken(log));
    chmodSync(join(bin, "aiken"), 0o755);
    const run = () =>
      spawnSync(
        "bash",
        [join(repo, "scripts/generate-user-events-witness-script-prefix.sh")],
        {
          cwd: repo,
          encoding: "utf8",
          env: { ...process.env, PATH: `${bin}:${process.env.PATH ?? ""}` },
        },
      );
    const calls = () => readFileSync(log, "utf8").trimEnd().split("\n");
    body({ repo, run, calls });
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};

test("formats only the generated environments, never the whole project", () => {
  withTree(({ repo, run, calls }) => {
    const result = run();
    assert.equal(result.status, 0, result.stderr);
    assert.deepEqual(
      calls().filter((call) => call.startsWith("fmt")),
      ["fmt env/default.ak env/testnet.ak"],
    );
    assert.equal(
      readFileSync(join(repo, "config/deployments/env.ak.template"), "utf8"),
      `pub const user_events_witness_script_prefix: ByteArray =\n  #"${prefix}"\n`,
    );
  });
});
