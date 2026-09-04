import { mkdtempSync, readFileSync, symlinkSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  buildRemoveFraudulentBlockCliConfig,
  isCliEntrypoint,
  main,
  parseArgs,
} from "../src/bin.js";

const makeEmptyTransitionTraceEnvelope = async () => {
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const header: SDK.Header = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    ...counts,
    startTime: 10n,
    endTime: 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "11".repeat(28),
    operatorVkey: "22".repeat(28),
    protocolVersion: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      transaction_preimages: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      deposits: [],
      transition_trace: [],
      event_to_step: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
  return {
    header,
    headerHash,
    envelope: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};

describe("fault-proof CLI argument parsing", () => {
  it("parses transition-trace prepare and repeatable submit reference inputs", () => {
    const prepared = parseArgs([
      "node",
      "midgard-fault-proofs",
      "prepare-transition-trace",
      "--da-payload-envelope",
      "payload.cbor.json",
      "--header-hash",
      "33".repeat(28),
      "--output-dir",
      "proofs",
    ]);
    expect(prepared).toMatchObject({
      command: "prepare-transition-trace",
      daPayloadEnvelopePath: "payload.cbor.json",
      headerHash: "33".repeat(28),
      outputDir: "proofs",
    });

    const submitted = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-transition-trace-proof",
      "--transition-fault-proof",
      "proof.cbor",
      "--reference-input",
      `${"44".repeat(32)}#0`,
      "--reference-input",
      `${"55".repeat(32)}#1`,
    ]);
    expect(submitted).toMatchObject({
      command: "submit-transition-trace-proof",
      transitionFaultProofPath: "proof.cbor",
      referenceInputOutRefs: [`${"44".repeat(32)}#0`, `${"55".repeat(32)}#1`],
    });
  });

  it("dispatches authenticated transition-trace preparation before the legacy prepare gate", async () => {
    const fixture = await makeEmptyTransitionTraceEnvelope();
    const tempDir = mkdtempSync(join(tmpdir(), "transition-trace-bin-"));
    const envelopePath = join(tempDir, "payload.cbor.json");
    const outputDir = join(tempDir, "proofs");
    writeFileSync(
      envelopePath,
      JSON.stringify({ cborHex: fixture.envelope.toString("hex") }),
    );
    const previousArgv = process.argv;
    const stdout = vi
      .spyOn(process.stdout, "write")
      .mockImplementation(() => true);
    process.argv = [
      "node",
      "midgard-fault-proofs",
      "prepare-transition-trace",
      "--da-payload-envelope",
      envelopePath,
      "--header-hash",
      fixture.headerHash,
      "--output-dir",
      outputDir,
    ];
    try {
      await expect(main()).resolves.toBeUndefined();
    } finally {
      process.argv = previousArgv;
      stdout.mockRestore();
    }

    const plan = JSON.parse(
      readFileSync(join(outputDir, "plan.json"), "utf8"),
    ) as {
      readonly headerHash: string;
      readonly proofCount: number;
      readonly detections: readonly unknown[];
      readonly guidance: readonly unknown[];
    };
    expect(plan).toMatchObject({
      headerHash: fixture.headerHash,
      proofCount: 0,
      detections: [],
    });
    expect(plan.guidance).toHaveLength(4);
  });

  it("keeps legacy diagnostic inputs outside the authenticated transition-trace prepare lane", async () => {
    const previousArgv = process.argv;
    process.argv = [
      "node",
      "midgard-fault-proofs",
      "prepare-transition-trace",
      "--da-payload-envelope",
      "must-not-be-read.cbor",
      "--header-hash",
      "66".repeat(28),
      "--transactions-file",
      "caller-asserted.json",
    ];
    try {
      await expect(main()).rejects.toThrow(
        "accepts only authenticated retained-DA evidence",
      );
    } finally {
      process.argv = previousArgv;
    }
  });

  it("dispatches transition-trace submission outside the retired legacy boundary", async () => {
    const previousArgv = process.argv;
    process.argv = [
      "node",
      "midgard-fault-proofs",
      "submit-transition-trace-proof",
      "--blueprint",
      "must-not-be-read-blueprint.json",
      "--deployment-info",
      "must-not-be-read-deployment.json",
      "--thread-out-ref",
      `${"77".repeat(32)}#0`,
    ];
    try {
      await expect(main()).rejects.toThrow(
        "Missing required --transition-fault-proof",
      );
    } finally {
      process.argv = previousArgv;
    }
  });

  it("strictly decodes transition proofs and rejects duplicate live reference inputs before provider construction", async () => {
    const fixture = await makeEmptyTransitionTraceEnvelope();
    const tempDir = mkdtempSync(join(tmpdir(), "transition-trace-submit-bin-"));
    const proofPath = join(tempDir, "proof.cbor");
    writeFileSync(
      proofPath,
      Data.to(
        SDK.makeTransitionFaultProof({
          challengedHeaderHash: fixture.headerHash,
          header: fixture.header,
          fault: SDK.countFault("HeaderTotalCountMismatch"),
        }),
        SDK.TransitionFaultProof,
      ),
    );
    const duplicateReferenceInput = `${"88".repeat(32)}#0`;
    const previousArgv = process.argv;
    process.argv = [
      "node",
      "midgard-fault-proofs",
      "submit-transition-trace-proof",
      "--blueprint",
      "must-not-be-read-blueprint.json",
      "--deployment-info",
      "must-not-be-read-deployment.json",
      "--thread-out-ref",
      `${"77".repeat(32)}#0`,
      "--transition-fault-proof",
      proofPath,
      "--reference-input",
      duplicateReferenceInput,
      "--reference-input",
      duplicateReferenceInput,
    ];
    try {
      await expect(main()).rejects.toThrow(
        "--reference-input values must be unique",
      );
    } finally {
      process.argv = previousArgv;
    }
  });

  it("rejects the retired incompatible-output bypass", () => {
    expect(() =>
      parseArgs([
        "node",
        "midgard-fault-proofs",
        "prepare-double-spend",
        "--sample-double-spend",
        "--header-hash",
        "11".repeat(28),
        "--allow-incompatible-output",
      ]),
    ).toThrow("Unknown argument: --allow-incompatible-output");
  });

  it("maps remove-fraudulent-block live-node lease flags into submission config", () => {
    const parsed = parseArgs([
      "node",
      "midgard-fault-proofs",
      "remove-fraudulent-block",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--fraudulent-header-hash",
      "11".repeat(28),
      "--network",
      "Preprod",
      "--provider",
      "Kupmios",
      "--kupo-url",
      "http://kupo.test",
      "--ogmios-url",
      "ws://ogmios.test",
      "--wallet-seed-phrase-env",
      "PROVER_WALLET",
      "--fraud-category",
      "invalidRange",
      "--midgard-node-url",
      "http://midgard-node.test/",
      "--midgard-node-admin-key-env",
      "NODE_ADMIN_KEY",
      "--state-queue-lease-ttl-ms",
      "45000",
      "--no-await-confirmation",
    ]);

    expect(parsed.command).toBe("remove-fraudulent-block");
    const config = buildRemoveFraudulentBlockCliConfig(parsed);
    expect(config).toMatchObject({
      blueprintPath: "plutus.json",
      deploymentInfoPath: "deployment.json",
      fraudulentHeaderHash: "11".repeat(28),
      network: "Preprod",
      provider: "Kupmios",
      kupoUrl: "http://kupo.test",
      ogmiosUrl: "ws://ogmios.test",
      walletSeedPhraseEnv: "PROVER_WALLET",
      fraudCategory: "invalidRange",
      midgardNodeUrl: "http://midgard-node.test/",
      midgardNodeAdminKeyEnv: "NODE_ADMIN_KEY",
      stateQueueLeaseTtlMs: 45000,
      awaitConfirmation: false,
    });
  });

  it("parses invalid-range prepare and init category arguments", () => {
    const prepare = parseArgs([
      "node",
      "midgard-fault-proofs",
      "prepare-invalid-range",
      "--transactions-file",
      "block-transactions.json",
      "--header-hash",
      "33".repeat(28),
      "--block-slot",
      "1000",
      "--tx-id",
      "44".repeat(32),
    ]);

    expect(prepare).toMatchObject({
      command: "prepare-invalid-range",
      transactionsPath: "block-transactions.json",
      headerHash: "33".repeat(28),
      blockSlot: "1000",
      txId: "44".repeat(32),
    });

    const init = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-init",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--fraudulent-block-out-ref",
      `${"55".repeat(32)}#0`,
      "--fraud-category",
      "invalidRange",
    ]);

    expect(init.fraudCategory).toBe("invalidRange");

    const transitionTraceInit = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-init",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--fraudulent-block-out-ref",
      `${"55".repeat(32)}#0`,
      "--fraud-category",
      "transitionTrace",
    ]);

    expect(transitionTraceInit.fraudCategory).toBe("transitionTrace");

    const validationTraceDisputeInit = parseArgs([
      "node",
      "bin",
      "submit-init",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--fraudulent-block-out-ref",
      `${"ef".repeat(32)}#0`,
      "--fraud-category",
      "validationTraceDispute",
    ]);
    expect(validationTraceDisputeInit.fraudCategory).toBe(
      "validationTraceDispute",
    );

    const nonExistentInputNoIndexInit = parseArgs([
      "node",
      "bin",
      "submit-init",
      "--fraud-category",
      "nonExistentInputNoIndex",
    ]);
    expect(nonExistentInputNoIndexInit.fraudCategory).toBe(
      "nonExistentInputNoIndex",
    );
  });

  it("rejects unknown fault-proof categories", () => {
    expect(() =>
      parseArgs([
        "node",
        "midgard-fault-proofs",
        "submit-init",
        "--fraud-category",
        "invalid-range",
      ]),
    ).toThrow(
      `--fraud-category must be one of ${SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => `"${category}"`).join(", ")}.`,
    );
  });

  it("accepts no-index removal now that the input-no-idx machine is registered", () => {
    const parsed = parseArgs([
      "node",
      "midgard-fault-proofs",
      "remove-fraudulent-block",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--fraudulent-header-hash",
      "11".repeat(28),
      "--fraud-category",
      "nonExistentInputNoIndex",
    ]);

    expect(buildRemoveFraudulentBlockCliConfig(parsed).fraudCategory).toBe(
      "nonExistentInputNoIndex",
    );
  });

  it("parses the four input-no-idx submit verbs and their preimage inputs", () => {
    const base = ["node", "midgard-fault-proofs"];
    const step01 = parseArgs([
      ...base,
      "submit-input-no-idx-step-01",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--thread-out-ref",
      `${"aa".repeat(32)}#0`,
      "--state-queue-block-out-ref",
      `${"bb".repeat(32)}#1`,
      "--tx-inclusion",
      "bad-tx-inclusion.json",
    ]);
    expect(step01.command).toBe("submit-input-no-idx-step-01");
    expect(step01.txInclusionPath).toBe("bad-tx-inclusion.json");

    const step02 = parseArgs([
      ...base,
      "submit-input-no-idx-step-02",
      "--thread-out-ref",
      `${"aa".repeat(32)}#0`,
      "--inputs-preimage",
      "inputs-preimage.json",
    ]);
    expect(step02.command).toBe("submit-input-no-idx-step-02");

    const fold = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-input-no-idx-fold",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--thread-out-ref",
      "ab".repeat(32) + "#0",
      "--inputs-preimage",
      "inputs.json",
      "--wallet-seed-phrase-env",
      "MIDGARD_PROVER_SEED",
    ]);
    expect(fold.command).toBe("submit-input-no-idx-fold");
    expect(fold.inputsPreimagePath).toBe("inputs.json");
    expect(fold.walletSeedPhraseEnv).toBe("MIDGARD_PROVER_SEED");
    expect(step02.inputsPreimagePath).toBe("inputs-preimage.json");

    const step03 = parseArgs([
      ...base,
      "submit-input-no-idx-step-03",
      "--thread-out-ref",
      `${"aa".repeat(32)}#0`,
      "--state-queue-block-out-ref",
      `${"bb".repeat(32)}#1`,
      "--tx-inclusion",
      "producing-tx-inclusion.json",
    ]);
    expect(step03.command).toBe("submit-input-no-idx-step-03");
    expect(step03.txInclusionPath).toBe("producing-tx-inclusion.json");

    const step04 = parseArgs([
      ...base,
      "submit-input-no-idx-step-04",
      "--thread-out-ref",
      `${"aa".repeat(32)}#0`,
      "--outputs-preimage",
      "outputs-preimage.json",
    ]);
    expect(step04.command).toBe("submit-input-no-idx-step-04");
    expect(step04.outputsPreimagePath).toBe("outputs-preimage.json");
  });

  it("parses fail-closed validation-dispute submission inputs", () => {
    const open = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-validation-dispute-open",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--thread-out-ref",
      `${"11".repeat(32)}#0`,
      "--state-queue-block-out-ref",
      `${"22".repeat(32)}#1`,
      "--validation-claim-cbor",
      "claim.cbor",
      "--challenger-descriptor-cbor",
      "challenger.cbor",
    ]);
    expect(open).toMatchObject({
      command: "submit-validation-dispute-open",
      validationClaimCborPath: "claim.cbor",
      challengerDescriptorCborPath: "challenger.cbor",
    });

    const reveal = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-validation-dispute-reveal",
      "--validation-dispute-role",
      "challenger",
      "--validation-trace-proof-cbor",
      "midpoint.cbor",
    ]);
    expect(reveal).toMatchObject({
      command: "submit-validation-dispute-reveal",
      validationDisputeRole: "challenger",
      validationTraceProofCborPath: "midpoint.cbor",
    });

    const prepareResolution = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-validation-dispute-prepare-resolution",
      "--validation-boundary-evidence-cbor",
      "boundary.cbor",
    ]);
    expect(prepareResolution).toMatchObject({
      command: "submit-validation-dispute-prepare-resolution",
      validationBoundaryEvidenceCborPath: "boundary.cbor",
    });

    const stagedResolution = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-validation-dispute-semantic-resolution",
      "--validation-transition-cbor",
      "transition.cbor",
      "--validation-auxiliary-cbor",
      "auxiliary.cbor",
      "--validation-resolver-index",
      "5",
      "--validation-semantic-resolver-index",
      "13",
    ]);
    expect(stagedResolution).toMatchObject({
      command: "submit-validation-dispute-semantic-resolution",
      validationTransitionCborPath: "transition.cbor",
      validationAuxiliaryCborPath: "auxiliary.cbor",
      validationResolverIndex: "5",
      validationSemanticResolverIndex: "13",
    });

    const directResolution = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-validation-dispute-direct-resolution",
      "--validation-transition-cbor",
      "transition.cbor",
      "--validation-auxiliary-cbor",
      "auxiliary.cbor",
      "--validation-resolver-index",
      "12",
    ]);
    expect(directResolution).toMatchObject({
      command: "submit-validation-dispute-direct-resolution",
      validationTransitionCborPath: "transition.cbor",
      validationAuxiliaryCborPath: "auxiliary.cbor",
      validationResolverIndex: "12",
    });

    expect(() =>
      parseArgs([
        "node",
        "midgard-fault-proofs",
        "submit-validation-dispute-reveal",
        "--validation-dispute-role",
        "either",
      ]),
    ).toThrow(/must be either "operator" or "challenger"/u);
  });

  it("accepts the zeroInput fault-proof category", () => {
    expect(
      parseArgs([
        "node",
        "midgard-fault-proofs",
        "submit-init",
        "--fraud-category",
        "zeroInput",
      ]).fraudCategory,
    ).toBe("zeroInput");
  });

  it("requires the counted header root for zero-input preparation", async () => {
    const previousArgv = process.argv;
    process.argv = [
      "node",
      "midgard-fault-proofs",
      "prepare-zero-input",
      "--transactions-file",
      "block-transactions.json",
      "--header-hash",
      "33".repeat(28),
    ];
    try {
      await expect(main()).rejects.toThrow(
        "Missing required --expected-transactions-root <hex>.",
      );
    } finally {
      process.argv = previousArgv;
    }
  });

  it("parses non-existent-input prepare, init category, and submit-step arguments", () => {
    const prepare = parseArgs([
      "node",
      "midgard-fault-proofs",
      "prepare-non-existent-input",
      "--transactions-file",
      "block-transactions.json",
      "--header-hash",
      "33".repeat(28),
      "--bad-tx-id",
      "44".repeat(32),
      "--bad-input-index",
      "2",
      "--prev-utxos-root",
      "55".repeat(32),
    ]);

    expect(prepare).toMatchObject({
      command: "prepare-non-existent-input",
      transactionsPath: "block-transactions.json",
      headerHash: "33".repeat(28),
      badTxId: "44".repeat(32),
      badInputIndex: "2",
      prevUtxosRoot: "55".repeat(32),
    });

    const init = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-init",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--fraudulent-block-out-ref",
      `${"55".repeat(32)}#0`,
      "--fraud-category",
      "nonExistentInput",
    ]);

    expect(init.fraudCategory).toBe("nonExistentInput");

    const step02 = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-non-existent-input-step-02",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--thread-out-ref",
      `${"66".repeat(32)}#0`,
      "--inputs-preimage",
      "ne-inputs-preimage.json",
      "--bad-input-index",
      "0",
    ]);

    expect(step02).toMatchObject({
      command: "submit-non-existent-input-step-02",
      threadOutRef: `${"66".repeat(32)}#0`,
      inputsPreimagePath: "ne-inputs-preimage.json",
      badInputIndex: "0",
    });

    const step03 = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-non-existent-input-step-03",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--thread-out-ref",
      `${"77".repeat(32)}#0`,
      "--ledger-non-membership-proof",
      "ne-ledger-non-membership.json",
    ]);

    expect(step03).toMatchObject({
      command: "submit-non-existent-input-step-03",
      ledgerNonMembershipProofPath: "ne-ledger-non-membership.json",
    });

    const step04 = parseArgs([
      "node",
      "midgard-fault-proofs",
      "submit-non-existent-input-step-04",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
      "--thread-out-ref",
      `${"88".repeat(32)}#0`,
      "--txs-non-membership-proof",
      "ne-txs-non-membership.json",
    ]);

    expect(step04).toMatchObject({
      command: "submit-non-existent-input-step-04",
      txsNonMembershipProofPath: "ne-txs-non-membership.json",
    });
  });

  it("passes a direct midgard node admin key through only for removal", () => {
    const config = buildRemoveFraudulentBlockCliConfig(
      parseArgs([
        "node",
        "midgard-fault-proofs",
        "remove-fraudulent-block",
        "--blueprint",
        "plutus.json",
        "--deployment-info",
        "deployment.json",
        "--fraudulent-header-hash",
        "22".repeat(28),
        "--midgard-node-url",
        "http://midgard-node.test",
        "--midgard-node-admin-key",
        "secret-admin-key",
      ]),
    );

    expect(config.midgardNodeUrl).toBe("http://midgard-node.test");
    expect(config.midgardNodeAdminKey).toBe("secret-admin-key");
    expect(config.midgardNodeAdminKeyEnv).toBeUndefined();
  });

  it("requires the remove-fraudulent-block header hash before building config", () => {
    const parsed = parseArgs([
      "node",
      "midgard-fault-proofs",
      "remove-fraudulent-block",
      "--blueprint",
      "plutus.json",
      "--deployment-info",
      "deployment.json",
    ]);

    expect(() => buildRemoveFraudulentBlockCliConfig(parsed)).toThrow(
      "Missing required --fraudulent-header-hash",
    );
  });

  it("detects package-manager bin symlinks as CLI entrypoints", () => {
    const tempDir = mkdtempSync(join(tmpdir(), "midgard-fault-proofs-bin-"));
    const target = join(tempDir, "bin.js");
    const symlink = join(tempDir, "midgard-fault-proofs");
    const unrelated = join(tempDir, "unrelated.js");
    writeFileSync(target, "#!/usr/bin/env node\n");
    writeFileSync(unrelated, "#!/usr/bin/env node\n");
    symlinkSync(target, symlink);

    expect(
      isCliEntrypoint({
        moduleUrl: pathToFileURL(target).href,
        argvPath: symlink,
      }),
    ).toBe(true);
    expect(
      isCliEntrypoint({
        moduleUrl: pathToFileURL(target).href,
        argvPath: resolve(unrelated),
      }),
    ).toBe(false);
    expect(
      isCliEntrypoint({
        moduleUrl: pathToFileURL(target).href,
        argvPath: undefined,
      }),
    ).toBe(false);
  });
});
