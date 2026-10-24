import { type ChildProcess, spawn } from "node:child_process";
import { once } from "node:events";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { Duplex } from "node:stream";
import { fileURLToPath, pathToFileURL } from "node:url";

import { afterEach, expect, it } from "vitest";

import {
  CHILD_STATUS_ATTEMPT_ENV,
  childStatusClient,
} from "../src/devnet-stack/child-status-channel.js";

const children: ChildProcess[] = [];
const roots: string[] = [];
afterEach(async () => {
  for (const child of children.splice(0)) {
    if (child.exitCode !== null || child.signalCode !== null) continue;
    const exited = once(child, "exit");
    child.kill("SIGTERM");
    await exited;
  }
  for (const root of roots.splice(0))
    rmSync(root, { recursive: true, force: true });
});
const setup = () => {
  const root = mkdtempSync(join(tmpdir(), "child-status-test-"));
  roots.push(root);
  const script = join(root, "child.mjs");
  const source = fileURLToPath(
    new URL("../src/devnet-stack/child-status-channel.ts", import.meta.url),
  );
  writeFileSync(
    script,
    `
import { answerChildStatus } from ${JSON.stringify(pathToFileURL(source).href)};
import { writeSync } from "node:fs";
answerChildStatus({
  parse: (v) => v !== null && typeof v === "object" && typeof v.challengeId === "string" ? v : null,
  answer: async (v) => {
    if (v.exit) { process.exit(0); }
    if (v.rawOversize) {writeSync(3, JSON.stringify({challengeId:v.challengeId,pid:process.pid,value:"actual-child",padding:"x".repeat(16384)})+"\\n");return new Promise(()=>{});}
    if (v.delay) await new Promise(resolve => setTimeout(resolve,v.delay));
    return {challengeId:v.wrongChallenge ? "wrong" : v.challengeId,pid:v.wrongPid ? process.pid+1 : process.pid,value:v.oversize ? "x".repeat(16385) : "actual-child"};
  }
});
setInterval(() => {},1000);
`,
  );
  const child = spawn(
    process.execPath,
    ["--experimental-strip-types", script],
    {
      stdio: ["ignore", "ignore", "inherit", "pipe"],
      env: {
        PATH: process.env.PATH,
        [CHILD_STATUS_ATTEMPT_ENV]: "synthetic-attempt",
      },
    },
  );
  children.push(child);
  const pipe = child.stdio[3];
  if (!(pipe instanceof Duplex)) throw new Error("missing actual child pipe");
  const client = childStatusClient({
    pipe,
    request: (challengeId, expected) => ({
      challengeId,
      ...(typeof expected === "object" && expected !== null ? expected : {}),
    }),
    response: (value, challengeId) => {
      if (
        typeof value !== "object" ||
        value === null ||
        !("challengeId" in value) ||
        !("pid" in value) ||
        !("value" in value) ||
        value.challengeId !== challengeId ||
        value.pid !== child.pid ||
        value.value !== "actual-child"
      )
        return null;
      return { pid: value.pid, value: value.value };
    },
  });
  return { child, client };
};
it("answers a fresh challenge over the actual inherited child pipe", async () => {
  const { child, client } = setup();
  expect(await client.request({}, 2000)).toEqual({
    pid: child.pid,
    value: "actual-child",
  });
  client.close();
});
it.each(["wrongChallenge", "wrongPid", "oversize", "rawOversize"])(
  "refuses actual-child %s frames",
  async (mode) => {
    const { client } = setup();
    expect(await client.request({ [mode]: true }, 2000)).toBeNull();
    expect(await client.request({}, 2000)).toBeNull();
  },
);
it("never reuses a late ready answer and permits a fresh challenge after bounded old work", async () => {
  const { client } = setup();
  expect(await client.request({}, 2000)).not.toBeNull();
  expect(await client.request({ delay: 50 }, 5)).toBeNull();
  await new Promise((resolve) => setTimeout(resolve, 150));
  expect(await client.request({}, 2000)).not.toBeNull();
  client.close();
});
it("an actual child exit resolves an outstanding challenge as unknown", async () => {
  const { client } = setup();
  expect(await client.request({ exit: true }, 2000)).toBeNull();
  expect(await client.request({}, 2000)).toBeNull();
});
