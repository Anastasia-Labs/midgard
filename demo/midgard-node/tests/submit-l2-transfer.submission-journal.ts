import "./submit-l2-transfer.submit-l2-transfer-program.js";

import { mkdtemp, readdir, readFile, rm } from "node:fs/promises";
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
  ResumableNativeTransferSubmitError,
  submitL2TransferCommandProgram,
  TRANSFER_SUBMISSION_JOURNAL_DIR_ENV,
  transferSubmissionJournalPath,
} from "../src/commands/submit-l2-transfer.js";
import { NodeConfig } from "../src/services/config.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  admitted,
  connectionRefused,
  destination,
  fakeNode,
  sender,
  txStatus,
  utxos,
} from "./submit-l2-transfer.fake-node.js";
import {
  launchDeploymentIdentity,
  OTHER_TEST_SEED,
  TEST_SEED,
} from "./submit-l2-transfer.submit-l2-transfer-config-helpers.js";

const SUBMISSION_ID = "journey-transfer-1";

describe("submit-l2-transfer submission journal", () => {
  let journalDir: string;

  beforeEach(async () => {
    journalDir = await mkdtemp(join(tmpdir(), "midgard-l2-transfer-journal-"));
  });

  afterEach(async () => {
    vi.unstubAllEnvs();
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    await rm(journalDir, { recursive: true, force: true });
  });

  type TransferChange = {
    readonly seed?: string;
    readonly l2Address?: string;
    readonly lovelace?: string;
    readonly journaled?: boolean;
  };

  const transfer = ({
    seed = TEST_SEED,
    l2Address = destination.address,
    lovelace = "3000000",
    journaled = true,
  }: TransferChange = {}) =>
    submitL2TransferCommandProgram({
      config: parseSubmitL2TransferConfig({
        l2Address,
        lovelace,
        assetSpecs: [],
        nodeEndpoint: "http://127.0.0.1:3000",
      }),
      ...(journaled
        ? { submission: { submissionId: SUBMISSION_ID, journalDir } }
        : {}),
      resolvedWalletSeedPhrase: resolveWalletSeedPhrase({
        walletSeedPhrase: seed,
        walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
        env: {},
      }),
      apiSubmitRetryPolicy: journaled
        ? {
            maxAttempts: 2,
            initialDelayMs: 0,
            maxDelayMs: 0,
            sleep: async () => {},
          }
        : undefined,
    }).pipe(
      Effect.provideService(
        ContractDeploymentIdentity,
        launchDeploymentIdentity,
      ),
      Effect.provide(NodeConfig.layer),
    );

  const run = (change?: TransferChange) => Effect.runPromise(transfer(change));

  /** The typed failure, unwrapped from the fiber failure. */
  const failureOf = (change?: TransferChange) =>
    Effect.runPromise(Effect.flip(transfer(change)));

  const readJournal = async () =>
    JSON.parse(
      await readFile(
        transferSubmissionJournalPath(journalDir, SUBMISSION_ID),
        "utf8",
      ),
    ) as {
      readonly transfer: {
        readonly txId: string;
        readonly signedTxCbor: string;
      };
    };

  it("journals the signed transfer before submitting and resubmits the exact bytes when rerun after a refused connection", async () => {
    const first = fakeNode({
      utxos: [utxos],
      submit: [connectionRefused, connectionRefused],
    });
    const failure = await failureOf();
    expect(failure).toBeInstanceOf(ResumableNativeTransferSubmitError);
    expect(String(failure)).toContain("transport failed");
    expect(String(failure)).toContain(
      `rerun submit-l2-transfer with --submission-id ${SUBMISSION_ID}`,
    );
    const journal = await readJournal();
    expect(first.submittedBodies()).toEqual([
      journal.transfer.signedTxCbor,
      journal.transfer.signedTxCbor,
    ]);

    const second = fakeNode({
      "tx-status": [txStatus("not_found")],
      submit: [admitted(202)],
    });
    const result = await run();

    // No /utxos request: the rerun neither selected inputs nor signed.
    expect(second.routes()).toEqual(["tx-status", "submit"]);
    expect(second.submittedBodies()).toEqual([journal.transfer.signedTxCbor]);
    expect(result).toMatchObject({
      submissionId: SUBMISSION_ID,
      txId: journal.transfer.txId,
      signedTxCbor: journal.transfer.signedTxCbor,
      status: "queued",
      senderAddress: sender.address,
      destinationAddress: destination.address,
      selectedInputs: [`${"44".repeat(32)}#0`],
      requestedAssets: { lovelace: 3_000_000n },
    });
  });

  it("returns the saved result without a second POST once the node knows the transfer", async () => {
    fakeNode({ utxos: [utxos], submit: [admitted(202)] });
    const original = await run();
    expect(original.txId).toBe((await readJournal()).transfer.txId);

    const second = fakeNode({ "tx-status": [txStatus("committed")] });
    const resumed = await run();

    expect(second.routes()).toEqual(["tx-status"]);
    expect(resumed).toEqual({ ...original, status: "committed" });
  });

  it("accepts the node's duplicate answer for journaled bytes it admitted despite a 5xx", async () => {
    fakeNode({
      utxos: [utxos],
      submit: [
        async () =>
          new Response(
            JSON.stringify({ error: "Submit ingress capacity is full" }),
            { status: 503 },
          ),
      ],
    });
    const failure = await failureOf();
    expect(failure).toBeInstanceOf(ResumableNativeTransferSubmitError);
    expect(String(failure)).toContain("(503)");
    expect(String(failure)).toContain(`--submission-id ${SUBMISSION_ID}`);

    const journal = await readJournal();
    const second = fakeNode({
      "tx-status": [txStatus("not_found")],
      submit: [admitted(200)],
    });
    const result = await run();

    expect(second.submittedBodies()).toEqual([journal.transfer.signedTxCbor]);
    expect(result.txId).toBe(journal.transfer.txId);
  });

  it("fails resumably without submitting when the rerun cannot read /tx-status", async () => {
    fakeNode({
      utxos: [utxos],
      submit: [connectionRefused, connectionRefused],
    });
    await failureOf();

    const second = fakeNode({ "tx-status": [connectionRefused] });
    const failure = await failureOf();

    expect(failure).toBeInstanceOf(ResumableNativeTransferSubmitError);
    expect(String(failure)).toContain(`--submission-id ${SUBMISSION_ID}`);
    expect(second.routes()).toEqual(["tx-status"]);
  });

  it.each([
    ["value", { lovelace: "3000001" }],
    ["destination", { l2Address: sender.address }],
    ["signer", { seed: OTHER_TEST_SEED }],
  ] as const)(
    "refuses to reuse a submission ID for a different %s",
    async (_label, change) => {
      fakeNode({ utxos: [utxos], submit: [admitted(202)] });
      await run();

      const second = fakeNode({});
      await expect(run(change)).rejects.toThrow(
        `Submission ID ${SUBMISSION_ID} belongs to a different transfer`,
      );
      expect(second.routes()).toEqual([]);
    },
  );

  it("without a submission ID builds and submits a new transfer on every run and journals nothing", async () => {
    vi.stubEnv(TRANSFER_SUBMISSION_JOURNAL_DIR_ENV, journalDir);
    const node = fakeNode({
      utxos: [utxos, utxos],
      submit: [admitted(202), admitted(202)],
    });

    for (let attempt = 0; attempt < 2; attempt += 1) {
      const result = await run({ journaled: false });
      expect(Object.keys(result).sort()).toEqual([
        "changeAssets",
        "destinationAddress",
        "nodeEndpoint",
        "requestedAssets",
        "selectedInputs",
        "senderAddress",
        "status",
        "txId",
        "walletSeedSource",
      ]);
    }

    expect(node.routes()).toEqual(["utxos", "submit", "utxos", "submit"]);
    expect(await readdir(journalDir)).toEqual([]);
  });

  it("without a submission ID keeps the single submit attempt", async () => {
    const node = fakeNode({ utxos: [utxos], submit: [connectionRefused] });

    const failure = await failureOf({ journaled: false });

    expect(String(failure)).toContain("transport failed");
    expect(String(failure)).not.toContain("--submission-id");
    expect(node.routes()).toEqual(["utxos", "submit"]);
  });
});
