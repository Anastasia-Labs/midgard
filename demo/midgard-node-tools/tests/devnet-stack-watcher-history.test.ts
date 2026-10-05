import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  createHistoryChainFollower,
  resumePoints,
} from "../src/devnet-stack/watcher-history-chain.js";
import { retainPayloadRecord } from "../src/devnet-stack/watcher-history-recorder.js";

const POLICY = "11".repeat(28);
const HEADER = "cd".repeat(28);

const scratches: string[] = [];
afterEach(() => {
  for (const dir of scratches.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const archiveDirectories = () => {
  const root = mkdtempSync(join(tmpdir(), "devnet-history-"));
  scratches.push(root);
  const directories = ["a", "b"].map((role) => {
    const directory = join(root, role);
    mkdirSync(join(directory, "canonical"), { recursive: true });
    return directory;
  });
  return { directories, commitsDirectory: join(root, "commits") };
};

const hashOf = (n: number) => n.toString(16).padStart(64, "0");

/** A transaction minting the state-queue node of `HEADER`: its commit. */
const commitTransaction = () => {
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(POLICY),
    CML.AssetName.from_hex(`${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${HEADER}`),
    1n,
  );
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    0n,
  );
  body.set_mint(mint);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    cbor: transaction.to_cbor_hex(),
    id: CML.hash_transaction(body).to_hex(),
  };
};

type Block = {
  blockHash: string;
  prevHash: string;
  slot: string;
  blockNo: string;
  transactionIds: string[];
  transactionCbors: string[];
};

const block = (
  n: number,
  transactions: { cbor: string; id: string }[] = [],
): Block => ({
  blockHash: hashOf(n),
  prevHash: hashOf(n - 1),
  slot: String(n * 10),
  blockNo: String(n),
  transactionIds: transactions.map(({ id }) => id),
  transactionCbors: transactions.map(({ cbor }) => cbor),
});

const forward = (value: Block) => ({ kind: "roll_forward", ...value }) as never;
const backward = (n: number | "origin") =>
  ({
    kind: "roll_backward",
    point:
      n === "origin"
        ? { kind: "origin" }
        : { kind: "point", blockHash: hashOf(n), slot: String(n * 10) },
  }) as never;

const follower = (paths: ReturnType<typeof archiveDirectories>) =>
  createHistoryChainFollower({
    ...paths,
    stateQueuePolicyId: POLICY,
    admit: (event) => event as never,
  });

const retained = (directory: string) =>
  readdirSync(join(directory, "canonical")).sort(
    (x, y) => parseInt(x) - parseInt(y),
  );

describe("history recorder chain follower", () => {
  it("resumes a restart from the retained chain instead of rebuilding it from origin", async () => {
    const paths = archiveDirectories();
    const first = follower(paths);
    expect(first.intersectionCandidates).toEqual([{ kind: "origin" }]);
    await first.onEvent(backward("origin"));
    for (let n = 1; n <= 5; n += 1) await first.onEvent(forward(block(n)));
    const files = ["1.json", "2.json", "3.json", "4.json", "5.json"];
    for (const directory of paths.directories)
      expect(retained(directory)).toEqual(files);
    const before = statSync(
      join(paths.directories[0]!, "canonical", "1.json"),
    ).mtimeMs;

    // The supervisor restarts it (SIGKILL, cardano-node restart).
    const restarted = follower(paths);
    expect(restarted.intersectionCandidates[0]).toEqual({
      kind: "point",
      blockHash: hashOf(5),
      slot: "50",
    });
    expect(restarted.intersectionCandidates).not.toContainEqual({
      kind: "origin",
    });
    // Chain-sync opens the session by acknowledging the intersection.
    await restarted.onEvent(backward(5));
    for (const directory of paths.directories)
      expect(retained(directory)).toEqual(files);
    expect(
      statSync(join(paths.directories[0]!, "canonical", "1.json")).mtimeMs,
    ).toBe(before);
    expect(restarted.latestBlockNo()).toBe(5n);
    await restarted.onEvent(forward(block(6)));
    expect(retained(paths.directories[1]!)).toContain("6.json");

    // Blocks the node rolled back while the recorder was down are dropped.
    const afterFork = follower(paths);
    await afterFork.onEvent(backward(4));
    for (const directory of paths.directories)
      expect(retained(directory)).toEqual(files.slice(0, 4));
  });

  it("resumes below a block whose update was cut short", async () => {
    const paths = archiveDirectories();
    const first = follower(paths);
    await first.onEvent(backward("origin"));
    for (let n = 1; n <= 5; n += 1) await first.onEvent(forward(block(n)));
    rmSync(join(paths.directories[1]!, "canonical-ready"));
    rmSync(join(paths.directories[1]!, "canonical", "3.json"));
    expect(
      resumePoints(paths.directories).map((point) => point.blockNo),
    ).toEqual(["1"]);
  });

  it("binds a header to the block that minted its node, across a restart and a rollback", async () => {
    const paths = archiveDirectories();
    const first = follower(paths);
    await first.onEvent(backward("origin"));
    await first.onEvent(forward(block(1)));
    await first.onEvent(forward(block(2, [commitTransaction()])));
    await first.onEvent(forward(block(3)));
    const committed = { blockHash: hashOf(2), blockNo: "2", slot: "20" };
    expect(first.commits.get(HEADER)).toEqual(committed);

    const restarted = follower(paths);
    await restarted.onEvent(backward(3));
    expect(restarted.commits.get(HEADER)).toEqual(committed);
    await restarted.onEvent(backward(1));
    expect(restarted.commits.get(HEADER)).toBeUndefined();
  });
});

describe("retainPayloadRecord", () => {
  it("copies the record a provider already holds instead of re-deriving it", () => {
    const { directories } = archiveDirectories();
    const [a, b] = directories.map((directory) =>
      join(directory, "record.json"),
    );
    writeFileSync(a!, "held");
    expect(
      retainPayloadRecord([a!, b!], () => {
        throw new Error("re-derived");
      }),
    ).toBe(true);
    expect(readFileSync(b!, "utf8")).toBe("held");
  });

  it("derives a missing record once and reports providers that disagree", () => {
    const { directories } = archiveDirectories();
    const [a, b] = directories.map((directory) =>
      join(directory, "record.json"),
    );
    expect(retainPayloadRecord([a!, b!], () => "fresh")).toBe(true);
    expect([readFileSync(a!, "utf8"), readFileSync(b!, "utf8")]).toEqual([
      "fresh",
      "fresh",
    ]);
    writeFileSync(b!, "other");
    expect(retainPayloadRecord([a!, b!], () => "fresh")).toBe(false);
    expect(existsSync(a!)).toBe(true);
  });
});
