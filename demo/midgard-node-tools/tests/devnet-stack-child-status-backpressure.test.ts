import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { Duplex } from "node:stream";
import { fileURLToPath, pathToFileURL } from "node:url";

import { expect, it } from "vitest";

import {
  CHILD_STATUS_ATTEMPT_ENV,
  CHILD_STATUS_MAX_FRAME_BYTES,
  childStatusClient,
} from "../src/devnet-stack/child-status-channel.js";

it("bounds queued frames when the actual owned child stops reading across timed-out challenges", async () => {
  const root = mkdtempSync(join(tmpdir(), "frame2-owned-backpressure-"));
  const file = join(root, "child.mjs");
  const source = fileURLToPath(
    new URL("../src/devnet-stack/child-status-channel.ts", import.meta.url),
  );
  writeFileSync(
    file,
    `import {answerChildStatus} from ${JSON.stringify(pathToFileURL(source).href)}; answerChildStatus({parse:v=>v,answer:async v=>({challengeId:v.challengeId,pid:process.pid})});setInterval(()=>{},1000);`,
  );
  const child = spawn(process.execPath, ["--experimental-strip-types", file], {
    stdio: ["ignore", "ignore", "inherit", "pipe"],
    env: {
      PATH: process.env.PATH,
      [CHILD_STATUS_ATTEMPT_ENV]: "independent-owned-attempt",
    },
  });
  const pipe = child.stdio[3];
  if (!(pipe instanceof Duplex)) throw new Error("Owned fd3 missing");
  const client = childStatusClient({
    pipe,
    request: (challengeId, expected) => ({ challengeId, padding: expected }),
    response: (v, id) =>
      typeof v === "object" &&
      v !== null &&
      "challengeId" in v &&
      v.challengeId === id &&
      "pid" in v &&
      v.pid === child.pid
        ? { pid: v.pid }
        : null,
  });
  let queued = 0;
  const queueBound = pipe.writableHighWaterMark + CHILD_STATUS_MAX_FRAME_BYTES;
  try {
    expect(await client.request("", 2000)).not.toBeNull();
    child.kill("SIGSTOP");
    for (let i = 0; i < 120; i++) {
      expect(await client.request("x".repeat(15000), 2)).toBeNull();
      queued = Math.max(queued, pipe.writableLength);
    }
    console.log(
      JSON.stringify({
        requests: 120,
        queuedBytes: queued,
        frameLimit: CHILD_STATUS_MAX_FRAME_BYTES,
        queueBound,
      }),
    );
    expect(pipe.writableNeedDrain).toBe(true);
    await new Promise((resolve) => setTimeout(resolve, 1100));
    expect(pipe.destroyed).toBe(true);
  } finally {
    child.kill("SIGCONT");
    client.close();
    const ended = once(child, "exit");
    child.kill("SIGTERM");
    const forced = setTimeout(() => child.kill("SIGKILL"), 500);
    try {
      await ended;
    } finally {
      clearTimeout(forced);
      rmSync(root, { recursive: true, force: true });
    }
  }
  expect(queued).toBeLessThanOrEqual(queueBound);
});

it("drains an accepted pressured write and requires a fresh nonce after the owned child resumes", async () => {
  const root = mkdtempSync(join(tmpdir(), "frame2-owned-drain-"));
  const file = join(root, "child.mjs");
  // The owned peer drains expired frames without publishing old ready replies.
  // This isolates output-pressure recovery from intentional attempt retirement
  // for an unsolicited reply outside the bounded expired-nonce set.
  writeFileSync(
    file,
    `import {Socket} from "node:net";const p=new Socket({fd:3,readable:true,writable:true});let pending="";p.on("data",b=>{pending+=b;for(let n;(n=pending.indexOf("\\n"))>=0;){const v=JSON.parse(pending.slice(0,n));pending=pending.slice(n+1);if(v.fresh)p.write(JSON.stringify({challengeId:v.challengeId,pid:process.pid})+"\\n");}});setInterval(()=>{},1000);`,
  );
  const child = spawn(process.execPath, [file], {
    stdio: ["ignore", "ignore", "inherit", "pipe"],
    env: { PATH: process.env.PATH },
  });
  const pipe = child.stdio[3];
  if (!(pipe instanceof Duplex)) throw new Error("Owned fd3 missing");
  const ids: string[] = [];
  const client = childStatusClient({
    pipe,
    request: (challengeId, fresh) => {
      ids.push(challengeId);
      return { challengeId, fresh, padding: "x".repeat(15000) };
    },
    response: (v, id) =>
      typeof v === "object" &&
      v !== null &&
      "challengeId" in v &&
      v.challengeId === id &&
      "pid" in v &&
      v.pid === child.pid
        ? { challengeId: id, pid: v.pid }
        : null,
  });
  try {
    expect(await client.request(true, 2000)).not.toBeNull();
    child.kill("SIGSTOP");
    for (let i = 0; i < 120 && !pipe.writableNeedDrain; i++)
      expect(await client.request(false, 2)).toBeNull();
    expect(pipe.writableNeedDrain).toBe(true);
    const sent = ids.length;
    expect(await client.request(true, 2000)).toBeNull();
    expect(ids).toHaveLength(sent);
    const drained = once(pipe, "drain");
    child.kill("SIGCONT");
    await drained;
    expect(await client.request(true, 2000)).toEqual({
      challengeId: ids[sent],
      pid: child.pid,
    });
    expect(ids[sent]).not.toBe(ids[sent - 1]);
  } finally {
    child.kill("SIGCONT");
    client.close();
    const ended = once(child, "exit");
    child.kill("SIGTERM");
    const forced = setTimeout(() => child.kill("SIGKILL"), 500);
    try {
      await ended;
    } finally {
      clearTimeout(forced);
      rmSync(root, { recursive: true, force: true });
    }
  }
});

it("starts one actual async handler while timed-out valid challenges are drained", async () => {
  const root = mkdtempSync(join(tmpdir(), "frame2-owned-handler-"));
  const file = join(root, "child.mjs");
  const source = fileURLToPath(
    new URL("../src/devnet-stack/child-status-channel.ts", import.meta.url),
  );
  writeFileSync(
    file,
    `import {answerChildStatus} from ${JSON.stringify(pathToFileURL(source).href)};let active=0,started=0;answerChildStatus({parse:v=>v,answer:async v=>{active++;started++;process.send({active,started});await new Promise(r=>setTimeout(r,200));active--;process.send({active,started});return {challengeId:v.challengeId,pid:process.pid};}});setInterval(()=>{},1000);`,
  );
  const child = spawn(process.execPath, ["--experimental-strip-types", file], {
    stdio: ["ignore", "ignore", "inherit", "pipe", "ipc"],
    env: {
      PATH: process.env.PATH,
      [CHILD_STATUS_ATTEMPT_ENV]: "owned-handler",
    },
  });
  const pipe = child.stdio[3];
  if (!(pipe instanceof Duplex)) throw new Error("Owned fd3 missing");
  const counts: unknown[] = [];
  child.on("message", (v) => counts.push(v));
  const client = childStatusClient({
    pipe,
    request: (challengeId) => ({ challengeId }),
    response: (v, id) =>
      typeof v === "object" &&
      v !== null &&
      "challengeId" in v &&
      v.challengeId === id &&
      "pid" in v &&
      v.pid === child.pid
        ? { pid: v.pid }
        : null,
  });
  try {
    const began = once(child, "message");
    const first = client.request(null, 100);
    await began;
    expect(await first).toBeNull();
    for (let i = 0; i < 20; i++)
      expect(await client.request(null, 2)).toBeNull();
    await new Promise((r) => setTimeout(r, 150));
    expect(counts).toEqual([
      { active: 1, started: 1 },
      { active: 0, started: 1 },
    ]);
    // More than eight expired replies retire an attempt rather than adopting
    // an old nonce. The single handler's late answer can never be ready.
    expect(await client.request(null, 2)).toBeNull();
  } finally {
    client.close();
    const ended = once(child, "exit");
    child.kill("SIGTERM");
    const forced = setTimeout(() => child.kill("SIGKILL"), 500);
    try {
      await ended;
    } finally {
      clearTimeout(forced);
      rmSync(root, { recursive: true, force: true });
    }
  }
});
