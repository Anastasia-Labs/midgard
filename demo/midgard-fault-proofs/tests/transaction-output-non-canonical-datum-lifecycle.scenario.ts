import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  planTransactionOutputDatum,
  submitTransactionOutputNonCanonicalStep01Forced,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep03,
} from "../src/transaction-output-non-canonical/index.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  buildForcedOutputFixture,
  outputWithInlineDatum,
  TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES,
} from "./support/transaction-output-non-canonical-emulator.js";
import {
  type Evidence,
  evidenceOf,
  MAXIMUM_OUTPUT_BYTES,
  type NativeTx,
  record,
  type Registered,
  registeredContracts,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/**
 * Datums and scenario builders for the datum step's emulator suite.
 */

/** Bytes before an inline datum of 256 bytes or more in `outputWithInlineDatum`'s shape. */
export const DATUM_OFFSET = 45;

export const hex = (value: string): Buffer => Buffer.from(value, "hex");

/** A canonical byte string of `chunks` full 64-byte chunks. */
export const chunkedBytes = (chunks: number): Buffer =>
  Buffer.concat([
    hex("5f"),
    ...Array.from({ length: chunks }, () =>
      Buffer.concat([hex("5840"), Buffer.alloc(64, 0x5a)]),
    ),
    hex("ff"),
  ]);

/**
 * A list holding 40 nested lists around a long chunked byte string and
 * `integers` one-byte integers, then `last`: the byte string crosses chunk
 * boundaries and the 41 open lists span several transactions.
 */
export const nestedDatum = (integers: number, last: Buffer): Buffer =>
  Buffer.concat([
    hex("9f"),
    Buffer.alloc(40, 0x9f),
    chunkedBytes(124),
    Buffer.alloc(integers, 0x00),
    Buffer.alloc(40, 0xff),
    last,
    hex("ff"),
  ]);

/** The last integer encoded with a needless one-byte argument: not canonical. */
export const NON_CANONICAL_DATUM = nestedDatum(300, hex("1805"));

/**
 * The canonical twin sized so the output ends exactly at its third chunk
 * boundary: the datum's last byte is the item's last byte.
 */
export const EXACT_TERMINAL_OUTPUT_BYTES =
  3 * TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES;
export const EXACT_TERMINAL_DATUM = nestedDatum(
  EXACT_TERMINAL_OUTPUT_BYTES - DATUM_OFFSET - nestedDatum(0, hex("05")).length,
  hex("05"),
);

/**
 * The costliest node shape at the largest datum an output can carry: nested
 * generic constructors (tag 102) whose alternative takes the widest integer
 * argument, each opening a frame. The levels fill the output exactly.
 */
export const GENERIC_OPEN = hex("d866821bffffffffffffffff9f");
export const GENERIC_DEPTH = Math.floor(
  (MAXIMUM_OUTPUT_BYTES - DATUM_OFFSET) / (GENERIC_OPEN.length + 1),
);
export const WORST_DENSITY_DATUM = Buffer.concat([
  ...Array.from({ length: GENERIC_DEPTH }, () => GENERIC_OPEN),
  Buffer.alloc(
    MAXIMUM_OUTPUT_BYTES -
      DATUM_OFFSET -
      GENERIC_DEPTH * (GENERIC_OPEN.length + 1),
  ),
  Buffer.alloc(GENERIC_DEPTH, 0xff),
]);

/** The most nodes a maximum-length datum holds: one-byte integers. */
export const MOST_NODES_DATUM = Buffer.concat([
  hex("9f"),
  Buffer.alloc(MAXIMUM_OUTPUT_BYTES - DATUM_OFFSET - 2, 0x00),
  hex("ff"),
]);

export const nodeTransaction = (output: Buffer) =>
  makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    referenceByte: "b1",
    outputCbors: [output],
  });

/** Every planned datum step for `datum`, run whole off chain. */
export const planOf = (datum: Buffer) =>
  planTransactionOutputDatum({
    item: outputWithInlineDatum(datum),
    datumOffset: DATUM_OFFSET,
    datumLength: datum.length,
  });

export const makeHarness = async () =>
  await makeFaultProofEmulatorHarness({
    contractOptions: {
      realTransactionOutputNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });

/**
 * A block whose forced leaf rejects output 0 of `transaction` for
 * `OutputNonCanonical`, and a builder for threads bound to that output and
 * scanned to the datum step.
 */
export const forcedDatumScenario = async (transaction: NativeTx) => {
  const harness = await makeHarness();
  const registered = await registeredContracts(harness);
  const funderCredential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (funderCredential?.type !== "Key")
    throw new Error("forced fixture funder key absent");
  const forced = await buildForcedOutputFixture({
    operatorVkey: funderCredential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    transaction,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: registered.catalogue,
    header: forced.header,
  });
  const certificateReference = await registered.publishReferences();
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  const evidence = evidenceOf(
    forcedVerdictSubject({
      transactionId: forced.transaction.tx_id,
      sourceKey: membership.key,
      rejectionReason: forced.rejectionReason,
    }),
    forced.nativeTx,
    0,
  );
  const source = {
    nativeTxCompactCbor: forced.transaction.submitted_source.compact_cbor,
    witnessSetCompactCbor:
      forced.transaction.submitted_source.witness_set_compact_cbor,
  };
  const bindToDatum = async (label?: string) => {
    const init = await registered.init(setup.fraudulentBlockOutRef);
    const bound = await submitTransactionOutputNonCanonicalStep01Forced({
      ...stepCommon(registered),
      threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
      finding: evidence,
      forcedSource: { header: forced.header, membership, direction: 1n },
      referenceScriptUtxo: registered.references[0]!,
    });
    const authenticated = await submitTransactionOutputNonCanonicalStep02({
      ...stepCommon(registered),
      ...source,
      threadOutRef: bound.nextThreadOutRef,
      evidence,
      referenceScriptUtxo: registered.references[1]!,
      certificateReferenceScriptUtxo: certificateReference,
    });
    return await scanToDatum(
      registered,
      authenticated.nextThreadOutRef,
      evidence,
      label,
    );
  };
  return { harness, registered, setup, evidence, bindToDatum };
};

export const stepCommon = (registered: Registered) => ({
  lucid: registered.harness.proverLucid,
  contracts: registered.contracts,
  categoryId: registered.category.categoryId,
  signer: registered.harness.proverSigner,
});

/** Runs the envelope scan; the canonical envelope opens the datum step. */
export const scanToDatum = async (
  registered: Registered,
  threadOutRef: string,
  evidence: Evidence,
  label?: string,
): Promise<string> => {
  let current = threadOutRef;
  for (;;) {
    const scanned = await captureEmulatorSubmission(
      registered.harness.emulator,
      () =>
        submitTransactionOutputNonCanonicalStep03({
          ...stepCommon(registered),
          threadOutRef: current,
          evidence,
          nativeTxCompactCbor: "",
          witnessSetCompactCbor: "",
          referenceScriptUtxo: registered.references[2]!,
        }),
    );
    current = scanned.result.nextThreadOutRef;
    if (scanned.result.terminal) {
      expect(scanned.result.nextStage).toBe("step03Datum");
      if (label !== undefined)
        record(`${label}-scan-to-datum`, scanned.measurement);
      return current;
    }
  }
};
