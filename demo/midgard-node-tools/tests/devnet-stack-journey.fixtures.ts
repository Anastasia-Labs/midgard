/** Shared fakes for the devnet journey tests: a deployment, a CLI and HTTP. */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { generateMnemonic } from "bip39";

import type { DeployContext } from "../src/devnet-stack/deploy.js";
import type { ExecResult } from "../src/devnet-stack/exec.js";
import {
  type CliRequest,
  Journey,
  type JourneyOptions,
} from "../src/devnet-stack/journey.js";
import type { HubOracleOneShot } from "../src/devnet-stack/node-env.js";

const dirs: string[] = [];
/** Removes every fake deployment directory; run after each test. */
export const removeFakeContexts = () => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
};

export const seeds = {
  userA: generateMnemonic(256),
  userB: generateMnemonic(256),
  userC: generateMnemonic(256),
};

/** Only what the journey reads of a deployment. */
export const fakeContext = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-journey-"));
  dirs.push(dir);
  return {
    layout: { journeyDir: join(dir, "journey"), nodeRoot: dir },
    run: { runId: "run1", kupoPort: 1442, portOffset: 0 },
    identities: { seeds },
  } as unknown as DeployContext;
};

let transcripts = 0;
export const exited = (code: number, stdout: unknown = ""): ExecResult => ({
  code,
  signal: null,
  stdout:
    typeof stdout === "string"
      ? stdout
      : `log line\n${JSON.stringify(stdout)}\n`,
  stderr: code === 0 ? "" : "boom",
  log: `/transcripts/${(transcripts += 1)}.log`,
});

export type Respond = (request: CliRequest) => ExecResult | Promise<ExecResult>;

/** A CLI runner that answers from a queue and records every request. */
export const scriptedCli = (...responses: Respond[]) => {
  const calls: CliRequest[] = [];
  const runCli = async (request: CliRequest) => {
    calls.push(request);
    const respond = responses.shift();
    if (respond === undefined)
      throw new Error(`unexpected CLI call ${request.label}`);
    return respond(request);
  };
  return { calls, runCli };
};

/** Never answers: the journey process dies while the command runs. */
export const crash: Respond = () => new Promise<ExecResult>(() => {});

export const json = (status: number, body: unknown) =>
  new Response(typeof body === "string" ? body : JSON.stringify(body), {
    status,
  });

export const makeJourney = (
  context: DeployContext,
  options: JourneyOptions = {},
) => {
  const logs: string[] = [];
  const journey = new Journey(context, {} as HubOracleOneShot, [], {
    sleep: async () => {},
    pollMs: 0,
    retryInitialMs: 1,
    retryMaxMs: 2,
    log: (message) => logs.push(message),
    ...options,
  });
  return { journey, logs };
};

export const flush = () => new Promise((resolve) => setTimeout(resolve, 20));

export const argAfter = (request: CliRequest, flag: string) =>
  request.args[request.args.indexOf(flag) + 1];
