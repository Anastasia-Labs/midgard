import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import {
  acceptanceExec,
  acceptanceJourneyOwner,
} from "../src/devnet-stack/acceptance-process.js";
import type { DeployContext } from "../src/devnet-stack/deploy.js";
import {
  type Identities,
  LIBP2P_IDENTITIES,
  USER_ROLES,
  WALLET_ROLES,
} from "../src/devnet-stack/identities.js";
import {
  type CliRequest,
  SUBMISSION_MARKER_ENV,
  SUBMISSION_RUN_ENV,
} from "../src/devnet-stack/journey-cli.js";
import { makeLayout } from "../src/devnet-stack/layout.js";
import {
  fakeContext,
  removeFakeContexts,
} from "./devnet-stack-journey.fixtures.js";

const dirs: string[] = [];
afterEach(() => {
  removeFakeContexts();
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

/** Stands in for a built CLI: reports which program ran and how. */
const stubCli = (program: string) =>
  `process.stdout.write(JSON.stringify({program:${JSON.stringify(program)},cwd:process.cwd(),args:process.argv.slice(2),env:process.env}))`;

/** A run whose node and `midgard-user` packages hold a stub CLI each. */
const stubbedStack = (): DeployContext => {
  const dir = realpathSync(
    mkdtempSync(join(tmpdir(), "devnet-acceptance-cli-")),
  );
  dirs.push(dir);
  const layout = {
    ...makeLayout(join(dir, "run")),
    nodeRoot: join(dir, "midgard-node"),
    userCliRoot: join(dir, "midgard-user-cli"),
  };
  for (const [root, entry, program] of [
    [layout.nodeRoot, "index.js", "node CLI"],
    [layout.userCliRoot, "cli.js", "midgard-user"],
  ] as const) {
    mkdirSync(join(root, "dist"), { recursive: true });
    writeFileSync(join(root, "dist", entry), stubCli(program));
  }
  return {
    layout,
    run: {
      runId: "run1",
      composeProject: "synthetic-acceptance",
      networkMagic: 42,
      ogmiosPort: 2337,
      kupoPort: 1442,
      postgresPort: 5432,
      postgresUser: "node-user",
      postgresPassword: "node-password",
      postgresDatabase: "node-db",
      cardanoImage: "unused",
      postgresImage: "unused",
      portOffset: 0,
    },
    identities: {
      schemaVersion: "midgard-devnet-identities-v1",
      seeds: Object.fromEntries(
        [...WALLET_ROLES, ...USER_ROLES].map((role) => [role, `seed-${role}`]),
      ),
      libp2p: Object.fromEntries(
        LIBP2P_IDENTITIES.map((id) => [id, "00".repeat(32)]),
      ),
      adminApiKey: "admin-key",
      publicReaderPassword: "reader",
    } as Identities,
    artifacts: {
      nativeOwnerBinary: "/artifacts/owner",
      nativeOwnerSha256: "owner-sha",
      transportBinary: "/artifacts/chain-sync",
    },
  };
};

/** Runs one request through the acceptance runner and reads the stub's report. */
const runThroughAcceptance = async (
  context: DeployContext,
  request: Pick<CliRequest, "tool" | "args" | "submissionId">,
) => {
  const owner = acceptanceJourneyOwner(
    context,
    { txHash: "ab".repeat(32), outputIndex: 1 },
    new AbortController().signal,
  );
  const result = await owner.options.runCli!({
    user: "userC",
    label: "stub",
    timeoutMs: 5000,
    ...request,
  });
  expect(result.stderr).toBe("");
  return JSON.parse(result.stdout) as {
    program: string;
    cwd: string;
    args: string[];
    env: Record<string, string | undefined>;
  };
};

it("runs a deposit through midgard-user with the user's environment, not the node's", async () => {
  const context = stubbedStack();
  const { layout } = context;
  const args = ["deposit", "--submission-id", "run1-deposit-C1", "--json"];
  const ran = await runThroughAcceptance(context, {
    tool: "user",
    args,
    submissionId: "run1-deposit-C1",
  });
  expect(ran.program).toBe("midgard-user");
  expect(ran.cwd).toBe(layout.userCliRoot);
  expect(ran.args).toEqual(args);
  expect(ran.env).toMatchObject({
    MIDGARD_DEPLOYMENT_MANIFEST: layout.contractManifest,
    L1_ACCESS: "kupmios",
    L1_KUPO_URL: "http://127.0.0.1:1442",
    L1_OGMIOS_URL: "http://127.0.0.1:2337",
    MIDGARD_USER_RECORD_DIR: join(layout.journeyDir, "user-submissions"),
    USER_SEED_PHRASE: "seed-userC",
    [SUBMISSION_RUN_ENV]: layout.journeyDir,
    [SUBMISSION_MARKER_ENV]: "run1-deposit-C1",
  });
  // The user's tool is handed nothing of the operator's.
  expect(ran.env.L1_OPERATOR_SEED_PHRASE).toBeUndefined();
  expect(ran.env.POSTGRES_PASSWORD).toBeUndefined();
  expect(ran.env.ADMIN_API_KEY).toBeUndefined();
});

it("runs a request that names no tool through the node CLI with the node's environment", async () => {
  const context = stubbedStack();
  const { layout } = context;
  const args = ["utxos", "--json"];
  const ran = await runThroughAcceptance(context, { args });
  expect(ran.program).toBe("node CLI");
  expect(ran.cwd).toBe(layout.nodeRoot);
  expect(ran.args).toEqual(args);
  expect(ran.env).toMatchObject({
    L1_ACCESS: "node",
    L1_OPERATOR_SEED_PHRASE: "seed-operator",
    POSTGRES_PASSWORD: "node-password",
    HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
    USER_SEED_PHRASE: "seed-userC",
  });
  expect(ran.env.MIDGARD_USER_RECORD_DIR).toBeUndefined();
  expect(ran.env[SUBMISSION_MARKER_ENV]).toBeUndefined();
});

it("preserves a real child failure code and joins its transcript", async () => {
  const context = fakeContext();
  const result = await acceptanceExec(
    process.execPath,
    ["-e", "process.stderr.write('exact child failure');process.exitCode=7"],
    {
      cwd: context.layout.nodeRoot,
      env: {},
      logDir: join(context.layout.nodeRoot, "logs"),
      label: "failure",
      signal: new AbortController().signal,
      timeoutMs: 5000,
    },
  );
  expect(result.code).toBe(7);
  expect(result.stderr).toBe("exact child failure");
  expect(readFileSync(result.log, "utf8")).toContain("exit code=7");
});

it("refuses a timed-out child even when its SIGTERM handler exits zero", async () => {
  const context = fakeContext();
  const pidFile = join(context.layout.nodeRoot, "pid");
  const task = acceptanceExec(
    process.execPath,
    [
      "-e",
      "process.on('SIGTERM',()=>process.exit(0));require('fs').writeFileSync(process.argv[1],String(process.pid));setInterval(()=>{},1000)",
      pidFile,
    ],
    {
      cwd: context.layout.nodeRoot,
      env: {},
      logDir: join(context.layout.nodeRoot, "logs"),
      label: "timeout",
      signal: new AbortController().signal,
      timeoutMs: 300,
    },
  );
  await expect(task).rejects.toThrow(/command timed out/);
  const pid = Number(readFileSync(pidFile, "utf8"));
  expect(pid).toBeGreaterThan(0);
  expect(() => process.kill(pid, 0)).toThrow();
});
