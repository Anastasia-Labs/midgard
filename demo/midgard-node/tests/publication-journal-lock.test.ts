import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { expect, it } from "vitest";

const lockModule = new URL(
  "./helpers/publication-journal-lock.ts",
  import.meta.url,
).href;
const contender = (path: string) => {
  const child = spawn(
    process.execPath,
    [
      "--experimental-strip-types",
      "--input-type=module",
      "-e",
      `
    import { acquirePublicationJournalLock } from ${JSON.stringify(lockModule)};
    try {
      const release = await acquirePublicationJournalLock(process.argv[1]);
      process.stdout.write('acquired\\n');
      process.stdin.resume();
      await new Promise(resolve => process.stdin.once('end', resolve));
      await release();
    } catch (cause) {
      process.stdout.write('busy\\n');
    }
  `,
      path,
    ],
    { stdio: ["pipe", "pipe", "pipe"] },
  );
  const result = once(child.stdout, "data").then(([chunk]) =>
    String(chunk).trim(),
  );
  return { child, result };
};

it("releases a killed owner's lock and admits only one of two concurrent restart processes", async () => {
  const path = join(
    await mkdtemp(join(tmpdir(), "publication-lock-")),
    "journal.lock",
  );
  const original = contender(path);
  expect(await original.result).toBe("acquired");
  const originalExited = once(original.child, "exit");
  original.child.kill("SIGKILL");
  await originalExited;
  const deadline = Date.now() + 5000;
  while (true) {
    const contenders = [contender(path), contender(path)];
    const results = await Promise.all(contenders.map(({ result }) => result));
    const winners = results.filter((result) => result === "acquired");
    expect(winners.length).toBeLessThanOrEqual(1);
    for (const { child } of contenders) child.stdin.end();
    await Promise.all(
      contenders.map(async ({ child }) => {
        if (child.exitCode === null) await once(child, "exit");
      }),
    );
    if (winners.length === 1) break;
    if (Date.now() >= deadline)
      throw new Error(
        "Killed publication owner did not release its kernel lock",
      );
    await pause(10);
  }
  const next = contender(path);
  expect(await next.result).toBe("acquired");
  const exited = once(next.child, "exit");
  next.child.stdin.end();
  await exited;
});
