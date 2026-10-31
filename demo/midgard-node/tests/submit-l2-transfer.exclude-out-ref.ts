import "./submit-l2-transfer.submission-journal.js";

import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  DEFAULT_WALLET_SEED_ENV,
  resolveWalletSeedPhrase,
} from "../src/commands/command-utils.js";
import {
  parseSubmitL2TransferConfig,
  submitL2TransferCommandProgram,
  transferSubmissionJournalPath,
} from "../src/commands/submit-l2-transfer.js";
import { NodeConfig } from "../src/services/config.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  admitted,
  destination,
  fakeNode,
  sender,
  senderUtxo,
  txStatus,
  utxosOf,
} from "./submit-l2-transfer.fake-node.js";
import {
  launchDeploymentIdentity,
  mkNodeUtxo,
  TEST_SEED,
} from "./submit-l2-transfer.submit-l2-transfer-config-helpers.js";

const SUBMISSION_ID = "journey-transfer-excluding";

/** Ranked below `senderUtxo` (8 ADA), so only an exclusion selects it. */
const smallerUtxo = mkNodeUtxo({
  txHash: "55".repeat(32),
  outputIndex: 1,
  address: sender.address,
  assets: { lovelace: 6_000_000n },
});
const senderLabel = `${"44".repeat(32)}#0`;
const smallerLabel = `${"55".repeat(32)}#1`;

describe("submit-l2-transfer --exclude-out-ref", () => {
  let journalDir: string;

  beforeEach(async () => {
    journalDir = await mkdtemp(join(tmpdir(), "midgard-l2-transfer-exclude-"));
  });

  afterEach(async () => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    await rm(journalDir, { recursive: true, force: true });
  });

  const transfer = (excludeOutRefs: readonly string[], journaled = true) =>
    submitL2TransferCommandProgram({
      config: parseSubmitL2TransferConfig({
        l2Address: destination.address,
        lovelace: "3000000",
        assetSpecs: [],
        nodeEndpoint: "http://127.0.0.1:3000",
        excludeOutRefs,
      }),
      ...(journaled
        ? { submission: { submissionId: SUBMISSION_ID, journalDir } }
        : {}),
      resolvedWalletSeedPhrase: resolveWalletSeedPhrase({
        walletSeedPhrase: TEST_SEED,
        walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
        env: {},
      }),
    }).pipe(
      Effect.provideService(
        ContractDeploymentIdentity,
        launchDeploymentIdentity,
      ),
      Effect.provide(NodeConfig.layer),
    );

  const run = (excludeOutRefs: readonly string[], journaled = true) =>
    Effect.runPromise(transfer(excludeOutRefs, journaled));

  const journalPath = () =>
    transferSubmissionJournalPath(journalDir, SUBMISSION_ID);

  it("never selects an excluded output, even the one selection would prefer", async () => {
    fakeNode({
      utxos: [utxosOf(senderUtxo, smallerUtxo)],
      submit: [admitted(202)],
    });
    expect((await run([], false)).selectedInputs).toEqual([senderLabel]);

    fakeNode({
      utxos: [utxosOf(senderUtxo, smallerUtxo)],
      submit: [admitted(202)],
    });
    expect((await run([senderLabel], false)).selectedInputs).toEqual([
      smallerLabel,
    ]);
  });

  it("names the exclusion when it leaves the sender nothing to spend", async () => {
    const node = fakeNode({ utxos: [utxosOf(senderUtxo)] });
    await expect(run([senderLabel], false)).rejects.toThrow(
      `No Midgard L2 UTxOs found for sender address ${sender.address} outside the 1 excluded by --exclude-out-ref.`,
    );
    expect(node.routes()).toEqual(["utxos"]);
  });

  it.each(["44#0#1", "44#0", `${"44".repeat(32)}#-1`])(
    "rejects the malformed label %s while parsing the command",
    (label) => {
      expect(() =>
        parseSubmitL2TransferConfig({
          l2Address: destination.address,
          lovelace: "3000000",
          assetSpecs: [],
          nodeEndpoint: "http://127.0.0.1:3000",
          excludeOutRefs: [senderLabel, label],
        }),
      ).toThrow(`Invalid --exclude-out-ref "${label}"`);
    },
  );

  it("journals the exclusions as intent: a rerun must name the same set", async () => {
    fakeNode({
      utxos: [utxosOf(senderUtxo, smallerUtxo)],
      submit: [admitted(202)],
    });
    await run([senderLabel]);
    const journal = JSON.parse(await readFile(journalPath(), "utf8")) as {
      readonly intent: { readonly excludedOutRefs: readonly string[] };
    };
    expect(journal.intent.excludedOutRefs).toEqual([senderLabel]);

    for (const changed of [[], [senderLabel, smallerLabel]]) {
      const refused = fakeNode({});
      await expect(run(changed)).rejects.toThrow(
        `Submission ID ${SUBMISSION_ID} belongs to a different transfer`,
      );
      expect(refused.routes()).toEqual([]);
    }

    // The same set, respelled, is the same intent.
    const resumed = fakeNode({ "tx-status": [txStatus("committed")] });
    const result = await run([` ${senderLabel.toUpperCase()} `, senderLabel]);
    expect(resumed.routes()).toEqual(["tx-status"]);
    expect(result.selectedInputs).toEqual([smallerLabel]);
  });

  it("reads a journal written before the option as excluding nothing", async () => {
    fakeNode({ utxos: [utxosOf(senderUtxo)], submit: [admitted(202)] });
    await run([]);
    const journal = JSON.parse(await readFile(journalPath(), "utf8")) as {
      intent: Record<string, unknown>;
    };
    delete journal.intent.excludedOutRefs;
    await writeFile(journalPath(), JSON.stringify(journal));

    const refused = fakeNode({});
    await expect(run([senderLabel])).rejects.toThrow(
      "belongs to a different transfer",
    );
    expect(refused.routes()).toEqual([]);

    fakeNode({ "tx-status": [txStatus("committed")] });
    expect((await run([])).status).toBe("committed");
  });
});
