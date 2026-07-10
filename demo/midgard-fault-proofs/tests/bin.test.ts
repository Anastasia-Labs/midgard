import { mkdtempSync, symlinkSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildRemoveFraudulentBlockCliConfig,
  isCliEntrypoint,
  parseArgs,
} from "../src/bin.js";

describe("fault-proof CLI argument parsing", () => {
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
      '--fraud-category must be one of "doubleSpend", "invalidRange", "transitionTrace", or "nonExistentInput"',
    );
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
