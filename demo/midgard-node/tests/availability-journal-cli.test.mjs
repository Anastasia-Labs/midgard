import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";
import { fileURLToPath } from "node:url";

import { test } from "vitest";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";

// Normal node Vitest cases invoke the actual compiled operator registry.
// The declared suite global prerequisites still apply to this offline command.
const cli = fileURLToPath(new URL("../dist/index.js", import.meta.url));
const fixture = () => {
  const directory = mkdtempSync(join(tmpdir(), "availability-holds-cli-"));
  return { directory, path: join(directory, "journal.sqlite") };
};
const run = (f, args) =>
  spawnSync(process.execPath, [cli, "availability-journal", "holds", ...args], {
    cwd: f.directory,
    env: {
      PATH: process.env.PATH,
      MIDGARD_DOTENV_MODE: "disabled",
      NODE_NO_WARNINGS: "1",
    },
    encoding: "utf8",
    timeout: 15_000,
    maxBuffer: 1024 * 1024,
  });
const events = (output) =>
  output
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line));

test("offline compiled holds lists real pending and lease-only residues absent from old status", () => {
  const f = fixture();
  const journal = openAvailabilityOperationJournal(f.path);
  try {
    const lease = journal.acquire("actor", "builder", 0, 100);
    journal.acquire("unsigned", "old-owner", 0, 10);
    journal.persist(
      lease,
      {
        id: "pending",
        actor: "actor",
        deploymentIdentity: "deployment",
        headerHash: "header",
        action: "publish",
        signedCbor: "PRIVATE-CBOR",
        txHash: "tx",
        spentOutRefs: ["input#0"],
        collateralOutRefs: [],
        expectedOutRefs: [],
        validUntilSlot: 10,
        completesWorkflow: false,
      },
      1,
    );
    // These are genuine persisted baseline residues, independent of the reader.
    assert.deepEqual(journal.unfinalized("deployment", "actor"), []);
    assert.equal(journal.pending("deployment", "actor").length, 1);
    assert.deepEqual(journal.reservedOutRefs("actor"), ["input#0"]);
    const result = run(f, ["--journal", f.path]);
    assert.equal(
      result.status,
      0,
      `inventory command must enumerate retained holds: ${result.stderr}`,
    );
    const observed = events(result.stdout);
    assert.equal(observed.at(-1).complete, true);
    assert.ok(
      observed.some(
        (page) =>
          page.family === "intents" &&
          page.rows.some((row) => row.id === "pending"),
      ),
    );
    assert.ok(
      observed.some(
        (page) =>
          page.family === "leases" &&
          page.rows.some((row) => row.actor === "unsigned"),
      ),
    );
    assert.equal(result.stdout.includes("PRIVATE-CBOR"), false);
    assert.equal(journal.get("pending").intent.signedCbor, "PRIVATE-CBOR");
  } finally {
    journal.close();
    rmSync(f.directory, { recursive: true, force: true });
  }
});

test("compiled holds refuses missing files without creating a journal", () => {
  const f = fixture();
  try {
    const result = run(f, ["--journal", f.path]);
    assert.equal(result.status, 1);
    assert.equal(events(result.stdout).at(-1).complete, false);
    assert.equal(
      events(result.stdout).at(-1).code,
      "inventory_existing_file_unavailable",
    );
    assert.equal(existsSync(f.path), false);
  } finally {
    rmSync(f.directory, { recursive: true, force: true });
  }
});

test("compiled holds preserves schema and legacy halt while refusing historical layouts", () => {
  const f = fixture();
  const journal = openAvailabilityOperationJournal(f.path);
  const db = new DatabaseSync(f.path);
  try {
    db.exec(
      "UPDATE availability_journal_metadata SET value = '2' WHERE key = 'schema'; INSERT INTO availability_journal_metadata VALUES ('halt','PRIVATE-HALT')",
    );
    const result = run(f, ["--journal", f.path]);
    assert.equal(result.status, 1);
    assert.equal(
      events(result.stdout).at(-1).code,
      "historical_schema_inventory_unsupported",
    );
    assert.equal(result.stdout.includes("PRIVATE-HALT"), false);
    assert.equal(
      db
        .prepare(
          "SELECT value FROM availability_journal_metadata WHERE key='schema'",
        )
        .get().value,
      "2",
    );
    assert.equal(
      db
        .prepare(
          "SELECT value FROM availability_journal_metadata WHERE key='halt'",
        )
        .get().value,
      "PRIVATE-HALT",
    );
  } finally {
    db.close();
    journal.close();
    rmSync(f.directory, { recursive: true, force: true });
  }
});

const fullPipeFixture = () => {
  const f = fixture();
  const journal = openAvailabilityOperationJournal(f.path);
  const db = new DatabaseSync(f.path);
  try {
    const insert = db.prepare(
      "INSERT INTO availability_operation_leases VALUES (?,?,1,0)",
    );
    db.exec("BEGIN");
    // More valid output than an OS pipe and parent Readable buffer can absorb.
    for (let index = 0; index < 4096; index++)
      insert.run(`actor-${String(index).padStart(4, "0")}`, "x".repeat(900));
    db.exec("COMMIT");
  } finally {
    db.close();
    journal.close();
  }
  return f;
};

const pipeChild = (f, pause, deadlineMs) => {
  const child = spawn(
    process.execPath,
    [cli, "availability-journal", "holds", "--journal", f.path],
    {
      cwd: f.directory,
      env: {
        PATH: process.env.PATH,
        MIDGARD_DOTENV_MODE: "disabled",
        NODE_NO_WARNINGS: "1",
      },
      stdio: ["ignore", "pipe", "pipe"],
    },
  );
  let stdout = "";
  let stderr = "";
  let firstOutput;
  let releaseFirst;
  const first = new Promise((resolve) => {
    releaseFirst = resolve;
  });
  child.stdout.on("data", (data) => {
    stdout += data.toString();
    if (!firstOutput) {
      firstOutput = true;
      if (pause) child.stdout.pause();
      releaseFirst();
    }
  });
  child.stderr.on("data", (data) => {
    stderr += data.toString();
  });
  // Physical child/stdio close is joined even on assertion failures.
  child.once("exit", () => {
    child.stdout.resume();
  });
  const joined = new Promise((resolve, reject) => {
    child.once("error", reject);
    child.once("close", (code, signal) => {
      clearTimeout(timeout);
      resolve({ code, signal, stdout, stderr });
    });
  });
  let timedOut = false;
  const timeout = setTimeout(() => {
    timedOut = true;
    child.kill("SIGKILL");
    child.stdout.resume();
  }, deadlineMs);
  return {
    child,
    first,
    joined,
    timedOut: () => timedOut,
    async cleanup() {
      if (child.exitCode === null && child.signalCode === null)
        child.kill("SIGKILL");
      child.stdout.resume();
      await joined;
    },
  };
};

for (const signal of ["SIGINT", "SIGTERM"]) {
  test(`compiled holds terminates blocked stdout on the first ${signal}`, async () => {
    const f = fullPipeFixture();
    const run = pipeChild(f, true, 15_000);
    try {
      await Promise.race([
        run.first,
        run.joined.then(() => {
          throw new Error("child exited before its first inventory page");
        }),
      ]);
      await new Promise((resolve) => setTimeout(resolve, 150));
      assert.equal(run.child.exitCode, null);
      assert.equal(run.child.kill(signal), true);
      const result = await run.joined;
      assert.equal(
        run.timedOut(),
        false,
        "first signal must terminate despite the pending output callback",
      );
      assert.equal(result.code, 1);
      assert.equal(result.signal, null);
      assert.equal(result.stdout.includes('"complete":true'), false);
      assert.match(result.stderr, /"code":"inventory_interrupted"/);
      assert.equal(result.stderr.includes("Error:"), false);
      const db = new DatabaseSync(f.path);
      try {
        assert.equal(
          db
            .prepare("SELECT COUNT(*) AS n FROM availability_operation_leases")
            .get().n,
          4096,
        );
      } finally {
        db.close();
      }
    } finally {
      await run.cleanup();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
}

test("compiled holds owns EPIPE and closes without raw stack or footer retries", async () => {
  const f = fullPipeFixture();
  const run = pipeChild(f, true, 15_000);
  try {
    await Promise.race([
      run.first,
      run.joined.then(() => {
        throw new Error("child exited before its first inventory page");
      }),
    ]);
    run.child.stdout.destroy();
    const result = await run.joined;
    assert.equal(run.timedOut(), false);
    assert.equal(result.code, 1);
    assert.equal(result.signal, null);
    assert.equal(result.stdout.includes('"complete":true'), false);
    assert.match(result.stderr, /"code":"inventory_output_failed"/);
    assert.equal(result.stderr.includes("Error:"), false);
    assert.equal(result.stderr.includes("EPIPE"), false);
    assert.equal(result.stderr.includes("emitUnhandledRejectionOrErr"), false);
  } finally {
    await run.cleanup();
    rmSync(f.directory, { recursive: true, force: true });
  }
});

test("compiled holds output deadline terminates a permanently paused pipe", async () => {
  const f = fullPipeFixture();
  const run = pipeChild(f, true, 75_000);
  try {
    await Promise.race([
      run.first,
      run.joined.then(() => {
        throw new Error("child exited before its first inventory page");
      }),
    ]);
    const result = await run.joined;
    assert.equal(
      run.timedOut(),
      false,
      "output deadline must settle the blocked action without an operator signal",
    );
    assert.equal(result.code, 1);
    assert.equal(result.signal, null);
    assert.equal(result.stdout.includes('"complete":true'), false);
    assert.match(result.stderr, /"code":"inventory_lifetime_exceeded"/);
    assert.equal(result.stderr.includes("Error:"), false);
  } finally {
    await run.cleanup();
    rmSync(f.directory, { recursive: true, force: true });
  }
});
