import { mkdtempSync, symlinkSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildRemoveFraudulentBlockCliConfig,
  isCliEntrypoint,
  main,
  parseArgs,
} from "../src/bin.js";

describe("fault-proof CLI argument parsing", () => {
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
      "--block-valid-from",
      "1000",
      "--block-valid-to",
      "2000",
      "--tx-id",
      "44".repeat(32),
    ]);

    expect(prepare).toMatchObject({
      command: "prepare-invalid-range",
      transactionsPath: "block-transactions.json",
      headerHash: "33".repeat(28),
      blockValidFrom: "1000",
      blockValidTo: "2000",
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
      '--fraud-category must be one of "doubleSpend", "invalidRange", "transitionTrace", "nonExistentInput", "nonExistentInputNoIndex", "zeroInput", "validationTraceDispute", or "daHashPreimage"',
    );
  });

  it("rejects no-index removal until its removal machine is implemented", () => {
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

    expect(() => buildRemoveFraudulentBlockCliConfig(parsed)).toThrow(
      /does not yet support the nonExistentInputNoIndex proof machine/u,
    );
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
