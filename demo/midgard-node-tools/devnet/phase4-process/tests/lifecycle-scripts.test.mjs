import assert from "node:assert/strict";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createServer } from "node:net";
import { join } from "node:path";
import { after, test } from "node:test";

import { root, run, temporaryRoot } from "./assets.run.mjs";

// up.sh and down.sh against a fake `docker` and `curl`: the argv they run,
// the environment compose sees, and the log followers they leave and stop.
// The caller's environment carries another run's settings throughout; every
// value compose sees must come from the run's own run.env.

const project = "midgard_phase4_process_lifecycle";
const scripts = join(root, "scripts");
const work = mkdtempSync(join(temporaryRoot, "phase4-lifecycle-"));
const bin = join(work, "bin");
const calls = join(work, "docker-calls.jsonl");
const runDir = join(work, "run");
let socket;

after(async () => {
  // A failed assertion must not leave a fake follower looping.
  for (const service of ["cardano-node", "ogmios", "kupo", "postgres"]) {
    const pidFile = join(runDir, "logs", `${service}.follower.pid`);
    if (!existsSync(pidFile)) continue;
    try {
      process.kill(-Number(readFileSync(pidFile, "utf8")), "SIGKILL");
    } catch {}
  }
  await new Promise((done) => (socket ? socket.close(done) : done()));
  rmSync(work, { recursive: true, force: true });
});

mkdirSync(bin);
// Compose interpolates compose.yaml's required variables for every
// subcommand, `down` included; the fake refuses the same way.
writeFileSync(
  join(bin, "docker"),
  `#!/bin/sh
if [ "$1" = compose ]; then
  : "\${MIDGARD_PHASE4_COMPOSE_PROJECT:?MIDGARD_PHASE4_COMPOSE_PROJECT is required}"
  : "\${MIDGARD_PHASE4_RUN_DIR:?MIDGARD_PHASE4_RUN_DIR is required}"
  : "\${MIDGARD_PHASE4_POSTGRES_PASSWORD:?MIDGARD_PHASE4_POSTGRES_PASSWORD is required}"
  : "\${MIDGARD_PHASE4_POSTGRES_DATABASE:?MIDGARD_PHASE4_POSTGRES_DATABASE is required}"
fi
node -e 'require("node:fs").appendFileSync(process.argv[1], JSON.stringify({ argv: process.argv.slice(2), env: Object.fromEntries(Object.entries(process.env).filter(([key]) => key.startsWith("MIDGARD_PHASE4_"))) }) + "\\n")' ${JSON.stringify(calls)} "$@"
for argument in "$@"; do
  if [ "$argument" = --follow ]; then
    echo "following"
    while :; do sleep 1; done
  fi
done
`,
  { mode: 0o755 },
);
writeFileSync(join(bin, "curl"), "#!/bin/sh\nexit 0\n", { mode: 0o755 });

mkdirSync(join(runDir, "cardano/ipc"), { recursive: true });
writeFileSync(
  join(runDir, "run.env"),
  [
    `MIDGARD_PHASE4_RUN_ID=lifecycle`,
    `MIDGARD_PHASE4_RUN_DIR=${runDir}`,
    `MIDGARD_PHASE4_COMPOSE_PROJECT=${project}`,
    `MIDGARD_PHASE4_NETWORK_MAGIC=42`,
    `MIDGARD_PHASE4_PROTOCOL_MAJOR=11`,
    `MIDGARD_PHASE4_OGMIOS_PORT=23370`,
    `MIDGARD_PHASE4_KUPO_PORT=24420`,
    `MIDGARD_PHASE4_POSTGRES_PORT=55440`,
    `MIDGARD_PHASE4_POSTGRES_USER=midgard_phase4_process`,
    `MIDGARD_PHASE4_POSTGRES_PASSWORD=run-password`,
    `MIDGARD_PHASE4_POSTGRES_DATABASE=midgard_phase4_process_lifecycle`,
    "",
  ].join("\n"),
  { mode: 0o600 },
);
writeFileSync(join(runDir, "compose.host-paths.json"), "{}\n");

// Another run's settings, left in the caller's shell.
const callerEnv = {
  PATH: `${bin}:${process.env.PATH}`,
  HOME: process.env.HOME ?? work,
  MIDGARD_PHASE4_RUN_DIR: join(work, "another-run"),
  MIDGARD_PHASE4_COMPOSE_PROJECT: "midgard_phase4_process_another",
  MIDGARD_PHASE4_POSTGRES_PASSWORD: "caller-password",
  MIDGARD_PHASE4_BLOCK_PRODUCER: "false",
};

const dockerCalls = () =>
  existsSync(calls)
    ? readFileSync(calls, "utf8")
        .trim()
        .split("\n")
        .map((line) => JSON.parse(line))
    : [];
const compose = [
  "compose",
  "--project-name",
  project,
  "--file",
  join(root, "compose.yaml"),
  "--file",
  join(runDir, "compose.host-paths.json"),
];
const alive = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

test("both commands refuse a missing or relative run directory", async () => {
  for (const script of ["up.sh", "down.sh"]) {
    const none = await run("sh", [join(scripts, script)], { env: callerEnv });
    assert.equal(none.status, 2);
    assert.match(none.stderr, /usage/);
    const relative = await run("sh", [join(scripts, script), "run"], {
      env: callerEnv,
    });
    assert.equal(relative.status, 1);
    assert.match(relative.stderr, /must be absolute/);
  }
  assert.deepEqual(dockerCalls(), []);
});

test("up starts the run's services from its run.env and follows their logs", async () => {
  socket = createServer();
  await new Promise((done) =>
    socket.listen(join(runDir, "cardano/ipc/node.socket"), done),
  );
  const result = await run("sh", [join(scripts, "up.sh"), runDir], {
    env: callerEnv,
    timeoutMs: 20_000,
  });
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /servicesStarted=true/);
  const [up, ...rest] = dockerCalls();
  assert.deepEqual(up.argv, [
    ...compose,
    "up",
    "--detach",
    "cardano-node",
    "ogmios",
    "kupo",
    "postgres",
  ]);
  assert.equal(up.env.MIDGARD_PHASE4_RUN_DIR, runDir);
  assert.equal(up.env.MIDGARD_PHASE4_COMPOSE_PROJECT, project);
  assert.equal(up.env.MIDGARD_PHASE4_POSTGRES_PASSWORD, "run-password");
  assert.equal(up.env.MIDGARD_PHASE4_BLOCK_PRODUCER, "true");
  const followed = rest
    .filter((call) => call.argv.includes("--follow"))
    .map((call) => call.argv.at(-1))
    .sort();
  assert.deepEqual(followed, ["cardano-node", "kupo", "ogmios", "postgres"]);
  for (const call of rest.filter((entry) => entry.argv.includes("--follow")))
    assert.deepEqual(call.argv.slice(0, compose.length), compose);
  // The socket is opened to the host user through a pinned helper image.
  assert.ok(rest.some((call) => call.argv[0] === "run"));
  for (const service of ["cardano-node", "ogmios", "kupo", "postgres"]) {
    const pid = Number(
      readFileSync(join(runDir, "logs", `${service}.follower.pid`), "utf8"),
    );
    assert.ok(alive(pid), `${service} follower is running`);
  }
});

test("down stops the run from its run.env, keeps its state and stops the followers", async () => {
  const pids = ["cardano-node", "ogmios", "kupo", "postgres"].map((service) =>
    Number(
      readFileSync(join(runDir, "logs", `${service}.follower.pid`), "utf8"),
    ),
  );
  writeFileSync(calls, "");
  const result = await run("sh", [join(scripts, "down.sh"), runDir], {
    env: callerEnv,
    timeoutMs: 20_000,
  });
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /statePreserved=/);
  const [down, ...rest] = dockerCalls();
  // Never --volumes / -v: down keeps the bind-mounted state.
  assert.deepEqual(down.argv, [...compose, "down"]);
  assert.deepEqual(rest, []);
  assert.equal(down.env.MIDGARD_PHASE4_RUN_DIR, runDir);
  assert.equal(down.env.MIDGARD_PHASE4_COMPOSE_PROJECT, project);
  assert.equal(down.env.MIDGARD_PHASE4_POSTGRES_PASSWORD, "run-password");
  assert.equal(
    down.env.MIDGARD_PHASE4_POSTGRES_DATABASE,
    "midgard_phase4_process_lifecycle",
  );
  const deadline = Date.now() + 5_000;
  while (pids.some(alive) && Date.now() < deadline)
    await new Promise((done) => setTimeout(done, 50));
  assert.deepEqual(pids.filter(alive), []);
  assert.ok(existsSync(join(runDir, "run.env")));
  assert.match(
    readFileSync(join(runDir, "logs", "kupo.log"), "utf8"),
    /following/,
  );
  for (const service of ["cardano-node", "ogmios", "kupo", "postgres"])
    assert.ok(!existsSync(join(runDir, "logs", `${service}.follower.pid`)));
});

test("down never signals a recorded pid that is no longer this run's follower", async () => {
  mkdirSync(join(runDir, "logs"), { recursive: true });
  writeFileSync(join(runDir, "logs", "kupo.follower.pid"), `${process.pid}\n`);
  const result = await run("sh", [join(scripts, "down.sh"), runDir], {
    env: callerEnv,
    timeoutMs: 20_000,
  });
  assert.equal(result.status, 0, result.stderr);
  assert.ok(alive(process.pid));
  assert.ok(!existsSync(join(runDir, "logs", "kupo.follower.pid")));
});
