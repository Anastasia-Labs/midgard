import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import { deriveMidgardFieldGrammarCertificate } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  buildFieldGrammarCertificateValidator,
  buildUnsignedFieldGrammarCertificateOpenProgram,
  buildUnsignedFieldGrammarCertificateReclaimProgram,
  fieldGrammarCertificateDatumCbor,
  fieldGrammarCertificateUnit,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  FIELD_GRAMMAR_NARROW_BATCH,
  fieldGrammarCertificateAddress,
  fieldGrammarOfCarriagePlan,
  planFieldGrammarBatches,
  reuseOrCertifyFaultProofFieldGrammar,
} from "../src/field-grammar-certificate.js";
import { faultProofRawFieldCarriage } from "../src/field-opening.publish-fault-proof-field-carriage.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
  VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
} from "../src/proof-fit/van-rossem.js";
import { resolveProverSigner } from "../src/runtime.js";
import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { alignEmulatorToWholeSecond } from "./support/emulator/emulator-context.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { pinStepTranscript } from "./support/step-transcript.js";

const EMPTY = Buffer.from("80", "hex");
/** The repository's per-transaction execution reserve: 80% of each limit. */
const RESERVED_MEMORY = (VAN_ROSSEM_MAX_MEMORY_UNITS * 4n) / 5n;
const RESERVED_CPU = (VAN_ROSSEM_MAX_CPU_UNITS * 4n) / 5n;

/**
 * A normal L2 transaction whose outputs field (2) is `items`, and nothing
 * else: 500 items of 9 bytes (one-byte wrappers) then 200 of 26 bytes, so the
 * batch sizer uses both batch classes and no single transaction certifies it.
 */
const sourceTransaction = (salt: number) => {
  const items = [
    ...Array.from({ length: 500 }, (_, k) => {
      const item = Buffer.alloc(9, salt);
      item.writeUInt16BE(k, 0);
      return item;
    }),
    ...Array.from({ length: 200 }, (_, k) => {
      const item = Buffer.alloc(26, salt);
      item.writeUInt16BE(k, 0);
      return item;
    }),
  ];
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY,
      referenceInputsPreimageCbor: EMPTY,
      outputsPreimageCbor: encodeMidgardFieldPreimage(items),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY,
      requiredSignersPreimageCbor: EMPTY,
      mintPreimageCbor: EMPTY,
      scriptIntegrityHash: Buffer.alloc(32),
      auxiliaryDataHash: Buffer.alloc(32),
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY,
      scriptTxWitsPreimageCbor: EMPTY,
      redeemerTxWitsPreimageCbor: EMPTY,
    },
  };
  const compact = deriveMidgardNativeTxCompact(
    canonical.body,
    canonical.witnessSet,
    canonical.validity,
  );
  return {
    txId: computeMidgardNativeTxId(compact),
    preimage: Buffer.from(canonical.body.outputsPreimageCbor),
    source: {
      sourceKind: 0n as const,
      compactCbor: encodeMidgardNativeTxCompact(compact).toString("hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
        deriveMidgardNativeTxWitnessSetCompact(canonical.witnessSet),
      ).toString("hex"),
    },
  };
};

/** An emulator account funded at its seed's enterprise (prover) address. */
const proverAccount = (lovelace: bigint) => {
  const account = generateEmulatorAccount({ lovelace });
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: account.seedPhrase,
  });
  return { account: { ...account, address: signer.address }, signer };
};

/** The inline datum of the certificate output of each signed transaction. */
const certificateDatums = (
  transactionCbors: readonly string[],
  certificateAddress: string,
): readonly string[] =>
  transactionCbors.map((cbor) => {
    const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const output = outputs.get(index);
      if (output.address().to_bech32() === certificateAddress) {
        const datum = output.datum()?.as_datum();
        if (datum !== undefined) return datum.to_cbor_hex();
      }
    }
    throw new Error("signed transaction carries no certificate datum");
  });

/**
 * One prover with the certificate's reference script published, and a field
 * of its own to certify. `open(batch, next)` builds an open of `batch` items
 * whose output carries the mirror's datum for `next`: the validator computes
 * the datum for `batch` itself, so any other `next` must be refused.
 */
const ownCertificateFixture = async (salt: number) => {
  const { account, signer } = proverAccount(5_000_000_000n);
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  alignEmulatorToWholeSecond(emulator);
  const lucid = await Lucid(emulator, "Custom");
  signer.selectWallet(lucid);
  const certificateValidator = buildFieldGrammarCertificateValidator({
    blueprint: parseFaultProofBlueprint(readBlueprint(realBlueprintPath)),
    network,
    fieldPreimageCertificatePolicyId: "ab".repeat(28),
  });
  const { utxo: certificateReferenceScriptUtxo } =
    await publishPlainReferenceScriptUtxo({
      lucid,
      script: certificateValidator.spendingScript,
      label: "field grammar certificate",
    });
  const { txId, preimage, source } = sourceTransaction(salt);
  const owner = Buffer.from(signer.paymentKeyHash, "hex");
  const plan = planMidgardFieldCarriage({
    owner,
    txId,
    fieldIndex: 2,
    preimage,
  });
  const grammar = fieldGrammarOfCarriagePlan(plan);
  const certificatePolicyId = certificateValidator.policyId;
  const certificateAddress = fieldGrammarCertificateAddress({
    network,
    certificatePolicyId,
  });
  const datumAt = (next: number) =>
    fieldGrammarCertificateDatumCbor(
      deriveMidgardFieldGrammarCertificate({ owner, txId, grammar, next }),
    );
  const open = async (batch: number, next: number) => {
    const opened = await Effect.runPromise(
      buildUnsignedFieldGrammarCertificateOpenProgram(lucid, {
        source,
        carriage: (referenceInputs) =>
          faultProofRawFieldCarriage({
            plan,
            referenceInputs,
            label: "field grammar certificate",
          }),
        batch,
        referenceInputs: [certificateReferenceScriptUtxo],
        certificatePolicyId,
        certificateAddress,
        datumCbor: datumAt(next),
        lovelace: 0n,
      }),
    );
    await lucid.awaitTx(
      await (await opened.sign.withWallet().complete()).submit(),
    );
  };
  return {
    lucid,
    signer,
    certificateValidator,
    certificateReferenceScriptUtxo,
    source,
    plan,
    grammar,
    certificateAddress,
    datumAt,
    open,
  };
};

describe("field grammar certificate builder against the emulator", () => {
  it("opens, advances and completes a certificate, reuses its own, and builds its own past another key's", async () => {
    pinStepTranscript();
    const prover = proverAccount(5_000_000_000n);
    const consumer = proverAccount(1_000_000_000n);
    const emulator = new Emulator(
      [prover.account, consumer.account],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    alignEmulatorToWholeSecond(emulator);
    const lucid = await Lucid(emulator, "Custom");
    const { signer } = prover;
    const consumerSigner = consumer.signer;
    signer.selectWallet(lucid);

    const certificateValidator = buildFieldGrammarCertificateValidator({
      blueprint: parseFaultProofBlueprint(readBlueprint(realBlueprintPath)),
      network,
      fieldPreimageCertificatePolicyId: "ab".repeat(28),
    });
    const publication = await captureEmulatorSubmission(emulator, () =>
      publishPlainReferenceScriptUtxo({
        lucid,
        script: certificateValidator.spendingScript,
        label: "field grammar certificate",
      }),
    );
    const certificateReferenceScriptUtxo = publication.result.utxo;
    expect(publication.measurement.completeSignedBytes).toBeLessThanOrEqual(
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
    );

    const { txId, preimage, source } = sourceTransaction(0x5a);
    const plan = planMidgardFieldCarriage({
      owner: Buffer.from(signer.paymentKeyHash, "hex"),
      txId,
      fieldIndex: 2,
      preimage,
    });
    expect(plan.tier).toBe("Inline");
    const grammar = fieldGrammarOfCarriagePlan(plan);
    expect(grammar.itemCount).toBe(700);
    expect(grammar.itemCount).toBeGreaterThan(FIELD_GRAMMAR_NARROW_BATCH);
    expect(planFieldGrammarBatches(grammar)).toEqual([384, 224, 92]);

    const certificateAddress = fieldGrammarCertificateAddress({
      network,
      certificatePolicyId: certificateValidator.policyId,
    });
    const input = {
      lucid,
      network,
      signer,
      plan,
      source,
      certificateValidator,
      certificateReferenceScriptUtxo,
    };
    const certified = await captureEmulatorSubmission(emulator, () =>
      reuseOrCertifyFaultProofFieldGrammar(input),
    );
    expect(certified.result.batches).toEqual([384, 224, 92]);
    expect(certified.result.txHashes).toHaveLength(3);
    expect(certified.measurements).toHaveLength(3);
    for (const measurement of certified.measurements) {
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
      );
      expect(measurement.executionMemory).toBeLessThanOrEqual(RESERVED_MEMORY);
      expect(measurement.executionSteps).toBeLessThanOrEqual(RESERVED_CPU);
    }
    console.info(
      "field grammar certificate per-batch cost",
      JSON.stringify({
        publicationSignedBytes: publication.measurement.completeSignedBytes,
        batches: certified.measurements.map((measurement, index) => ({
          batch: certified.result.batches[index],
          signedBytes: measurement.completeSignedBytes,
          mem: measurement.executionMemory.toString(),
          cpu: measurement.executionSteps.toString(),
        })),
      }),
    );

    // Every datum the validator accepted is the TS mirror's, byte for byte.
    const owner = Buffer.from(signer.paymentKeyHash, "hex");
    const mirrored = (next: number) =>
      fieldGrammarCertificateDatumCbor(
        deriveMidgardFieldGrammarCertificate({ owner, txId, grammar, next }),
      );
    expect(
      certificateDatums(certified.transactionCbors, certificateAddress),
    ).toEqual([mirrored(384), mirrored(608), mirrored(700)]);
    expect(certified.result.certificateUtxo.datum).toBe(mirrored(700));
    expect(
      certified.result.certificateUtxo.assets[
        fieldGrammarCertificateUnit(certificateValidator.policyId)
      ],
    ).toBe(1n);

    // Reuse: the prover's own live Complete certificate serves, and nothing
    // is sent.
    const submit = emulator.submitTx.bind(emulator);
    let submissions = 0;
    emulator.submitTx = async (transaction) => {
      submissions += 1;
      return submit(transaction);
    };
    try {
      const reused = await reuseOrCertifyFaultProofFieldGrammar(input);
      expect(reused.txHashes).toEqual([]);
      expect(reused.certificateUtxo.txHash).toBe(
        certified.result.certificateUtxo.txHash,
      );
      expect(reused.certificateUtxo.outputIndex).toBe(
        certified.result.certificateUtxo.outputIndex,
      );
    } finally {
      emulator.submitTx = submit;
    }
    expect(submissions).toBe(0);

    // Another key's Complete certificate does not serve: the step relies only
    // on a certificate the prover controls, so the consumer builds its own.
    const own = await reuseOrCertifyFaultProofFieldGrammar({
      ...input,
      signer: consumerSigner,
    });
    expect(own.batches).toEqual([384, 224, 92]);
    expect(own.certificateUtxo.txHash).not.toBe(
      certified.result.certificateUtxo.txHash,
    );
    expect(own.certificateUtxo.datum).toBe(
      fieldGrammarCertificateDatumCbor(
        deriveMidgardFieldGrammarCertificate({
          owner: Buffer.from(consumerSigner.paymentKeyHash, "hex"),
          txId,
          grammar,
          next: grammar.itemCount,
        }),
      ),
    );

    // The owner reclaims the deposit and the token is burnt.
    signer.selectWallet(lucid);
    const reclaim = await Effect.runPromise(
      buildUnsignedFieldGrammarCertificateReclaimProgram(lucid, {
        certificateUtxo: certified.result.certificateUtxo,
        certificatePolicyId: certificateValidator.policyId,
        ownerKeyHash: signer.paymentKeyHash,
        referenceInputs: [certificateReferenceScriptUtxo],
      }),
    );
    const reclaimHash = await (
      await reclaim.sign.withWallet().complete()
    ).submit();
    await lucid.awaitTx(reclaimHash);
    expect(
      (await lucid.utxosAt(certificateAddress)).map(({ txHash }) => txHash),
    ).toEqual([own.certificateUtxo.txHash]);
  }, 600_000);

  it("resumes its own InProgress certificate from the item it reached", async () => {
    const {
      lucid,
      signer,
      certificateValidator,
      certificateReferenceScriptUtxo,
      source,
      plan,
      grammar,
      certificateAddress,
      datumAt,
      open,
    } = await ownCertificateFixture(0x3c);

    // An interrupted run: only a 100-item open landed.
    await open(100, 100);

    const resumed = await reuseOrCertifyFaultProofFieldGrammar({
      lucid,
      network,
      signer,
      plan,
      source,
      certificateValidator,
      certificateReferenceScriptUtxo,
    });
    expect(resumed.batches).toEqual(planFieldGrammarBatches(grammar, 100));
    expect(resumed.batches).toEqual([384, 216]);
    expect(resumed.certificateUtxo.datum).toBe(datumAt(grammar.itemCount));
    expect(await lucid.utxosAt(certificateAddress)).toHaveLength(1);
  }, 600_000);

  it("refuses an open whose certificate claims items its batch did not walk", async () => {
    const { lucid, certificateAddress, open } =
      await ownCertificateFixture(0x5a);

    // 100 items walked, 116 claimed: one waypoint and sixteen items too many.
    await expect(open(100, 116)).rejects.toThrow(
      /Failed to open a field grammar certificate: .*failed script execution Mint\[0\]/u,
    );
    expect(await lucid.utxosAt(certificateAddress)).toHaveLength(0);
    // The same open with the datum the validator computes is accepted.
    await open(100, 100);
    expect(await lucid.utxosAt(certificateAddress)).toHaveLength(1);
  }, 600_000);
});
