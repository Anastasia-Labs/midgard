import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  isAnyNetworkIdMismatch,
  isExplicitTransactionNetworkMismatch,
  NETWORK_ID_FORCED_GRAMMAR_BATCH,
  NETWORK_ID_FORCED_SCAN_BATCH,
  NetworkIdFaultSchema,
  NetworkIdForcedScanActionSchema,
  NetworkIdForcedScanBoundSchema,
  NetworkIdForcedScanDatumSchema,
  NetworkIdForcedScanSpendRedeemerSchema,
  NetworkIdForcedScanStateSchema,
  NetworkIdForcedStepDatumSchema,
  NetworkIdForcedStepSpendRedeemerSchema,
  NetworkIdStep01SpendRedeemerSchema,
  NetworkIdStep02DatumSchema,
  NetworkIdStep02SpendRedeemerSchema,
  NetworkIdStep02StateSchema,
} from "../src/fraud-proof/network-id.js";
import type { Header } from "../src/ledger-state.js";

const h28 = "11".repeat(28);
const h32 = "22".repeat(32);

describe("Q35 network-id wire codec", () => {
  it("pins all claim constructor indices", () => {
    expect(Data.to("TransactionNetwork" as never, NetworkIdFaultSchema)).toBe(
      "d87980",
    );
    expect(
      Data.to(
        { OutputNetwork: { output_index: 11n } } as never,
        NetworkIdFaultSchema,
      ),
    ).toBe("d87a9f0bff");
    expect(
      Data.to(
        { OutputNetworkUtxo: { observed_network_id: 7n } } as never,
        NetworkIdFaultSchema,
      ),
    ).toBe("d87b9f07ff");
  });

  it("round-trips direct post-UTxO descriptor membership evidence", () => {
    const redeemer = {
      Continue: [
        {
          tx_inclusion: null,
          post_utxo_membership: {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            state_queue_node_ref_input_index: 1n,
            out_ref: { transactionId: h32, outputIndex: 11n },
            descriptor_cbor: "80",
            membership: {
              RedeemerCarriedMembership: {
                membership_proof: [],
                membership_proof_script_redeemer_index: 0n,
              },
            },
            predecessor: "Introduced",
          },
          forced_source: null,
          fault: { OutputNetworkUtxo: { observed_network_id: 7n } },
        },
      ],
    } as const;
    const encoded = Data.to(
      redeemer as never,
      NetworkIdStep01SpendRedeemerSchema,
    );
    expect(Data.from(encoded, NetworkIdStep01SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });

  it("round-trips the authenticated step-02 state", () => {
    const datum = {
      fraud_prover: h28,
      data: {
        bad_tx_id: h32,
        committed_tx_network_id: 1n,
        expected_network_id: 0n,
        fault: { OutputNetwork: { output_index: 11n } },
        post_utxo: null,
        forced_source_key: null,
      },
    } as const;
    const encoded = Data.to(datum as never, NetworkIdStep02DatumSchema);
    expect(Data.from(encoded, NetworkIdStep02DatumSchema)).toEqual(datum);
  });

  it("round-trips a complete inline field-2 opening", () => {
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
          outputs_opening: {
            BodyFieldOpening: {
              native_tx_compact_cbor: "80",
              carriage: { Inline: { preimage: "81" } },
            },
          },
          predecessor_carriage: null,
        },
      ],
    } as const;
    const encoded = Data.to(
      redeemer as never,
      NetworkIdStep02SpendRedeemerSchema,
    );
    expect(Data.from(encoded, NetworkIdStep02SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });

  it("does not misclassify Cardano's absent transaction network", () => {
    expect(
      isExplicitTransactionNetworkMismatch({
        committedNetworkId: 255n,
        expectedNetworkId: 0n,
      }),
    ).toBe(false);
    expect(
      isExplicitTransactionNetworkMismatch({
        committedNetworkId: 1n,
        expectedNetworkId: 0n,
      }),
    ).toBe(true);
  });
});

/**
 * Absolute wire pins for the forced door, measured against the Aiken family
 * modules through `cbor.serialise` and re-pinned verbatim in
 * `onchain/aiken/lib/midgard/fraud-proofs/network-id/forced-wire.test.ak`.
 */
const FORCED_STEP_REDEEMER_CBOR =
  "d87a9fd8799f0000d8799f582055555555555555555555555555555555555555555555555555555555555555555820666666666666666666666666666666666666666666666666666666666666666658200000000000000000000000000000000000000000000000000000000000000000582033333333333333333333333333333333333333333333333333333333333333335820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000000100000101000a1400000000581c99999999999999999999999999999999999999999999999999999999581c9999999999999999999999999999999999999999999999999999999901ffd8799fd87a80582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444401d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffd8799f58202222222222222222222222222222222222222222222222222222222222222222d8799f418041814182ffd87a9fd87e80ffff80ff01ffff";

const FORCED_STEP_02_STATE_CBOR =
  "d8799f5820222222222222222222222222222222222222222222222222222222222222222218ff00d87c80d87a80d8799f5827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffffff";

const FORCED_STEP_02_DATUM_CBOR =
  "d8799f581c11111111111111111111111111111111111111111111111111111111d8799fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff00d87c80d87a80d8799f5827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffffffffff";

/**
 * ## Forced-door wire twins
 *
 * Every literal below is shared verbatim with
 * `onchain/aiken/lib/midgard/fraud-proofs/network-id/forced-wire.test.ak`,
 * which pins the identical bytes through `cbor.serialise` over the Aiken
 * family types. Nothing here compares one TypeScript derivation against
 * another: if either language's encoding moves, both sides go red.
 */
describe("Q35 network-id forced-door wire twins", () => {
  const fraudProver = "11".repeat(28);
  const operatorVkey = "99".repeat(28);
  const badTxId = "22".repeat(32);
  const forcedTransactionsRoot = "33".repeat(32);
  const forcedTransactionsPhasRoot = "44".repeat(32);
  const orderKeyTransactionId = "77".repeat(32);
  const zeroRoot = "00".repeat(32);
  /** Canonical CBOR of the forced order key, as `forced_source_key` holds it. */
  const forcedSourceKey = `d8799f5820${orderKeyTransactionId}00ff`;

  const header: Header = {
    prevUtxosRoot: "55".repeat(32),
    utxosRoot: "66".repeat(32),
    withdrawalsRoot: zeroRoot,
    forcedTransactionsRoot,
    transactionsRoot: zeroRoot,
    depositsRoot: zeroRoot,
    transitionTraceRoot: zeroRoot,
    eventToStepRoot: zeroRoot,
    validationTracesRoot: zeroRoot,
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 0n,
    startTime: 10n,
    endTime: 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: operatorVkey,
    operatorVkey,
    protocolVersion: 1n,
  };

  const forcedStep02State = {
    bad_tx_id: badTxId,
    committed_tx_network_id: 255n,
    expected_network_id: 0n,
    fault: "ForcedNetworkIdMismatch",
    post_utxo: null,
    forced_source_key: forcedSourceKey,
  } as const;

  it("pins the forced handoff datum", () => {
    expect(
      Data.to(
        { fraud_prover: fraudProver, data: "ForcedNetworkIdMismatch" } as never,
        NetworkIdForcedStepDatumSchema,
      ),
    ).toBe(`d8799f581c${fraudProver}d8799fd87c80ffff`);
  });

  it("pins the empty forced handoff datum the door refuses", () => {
    expect(
      Data.to(
        { fraud_prover: fraudProver, data: null } as never,
        NetworkIdForcedStepDatumSchema,
      ),
    ).toBe(`d8799f581c${fraudProver}d87a80ff`);
  });

  it("pins the forced-step Continue redeemer", () => {
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          header,
          membership: {
            domain: "ForcedTransactionsV1RootDomain",
            root: forcedTransactionsRoot,
            phas_root: forcedTransactionsPhasRoot,
            count: 1n,
            key: { transactionId: orderKeyTransactionId, outputIndex: 0n },
            value: {
              tx_id: badTxId,
              submitted_source: {
                compact_cbor: "80",
                witness_set_compact_cbor: "81",
                field_preimage_lengths_cbor: "82",
              },
              verdict: { ForcedTxInvalid: { reason: "NetworkIdMismatch" } },
            },
            proof: [],
          },
          direction: 1n,
        },
      ],
    };
    expect(
      Data.to(redeemer as never, NetworkIdForcedStepSpendRedeemerSchema),
    ).toBe(FORCED_STEP_REDEEMER_CBOR);
  });

  it("pins the forced-flavoured terminal state and datum", () => {
    expect(
      Data.to(forcedStep02State as never, NetworkIdStep02StateSchema),
    ).toBe(FORCED_STEP_02_STATE_CBOR);
    expect(
      Data.to(
        { fraud_prover: fraudProver, data: forcedStep02State } as never,
        NetworkIdStep02DatumSchema,
      ),
    ).toBe(FORCED_STEP_02_DATUM_CBOR);
  });

  it("refuses to acquit a forced leaf whose outputs really mismatch", () => {
    expect(
      isAnyNetworkIdMismatch({
        committedNetworkId: 255n,
        outputNetworkIds: [0n, 0n],
        expectedNetworkId: 0n,
      }),
    ).toBe(false);
    expect(
      isAnyNetworkIdMismatch({
        committedNetworkId: 255n,
        outputNetworkIds: [0n, 1n],
        expectedNetworkId: 0n,
      }),
    ).toBe(true);
  });
});

/**
 * ## Forced-scan wire twins
 *
 * The §10 scan's thread states and redeemer actions, pinned byte-for-byte
 * against `forced-wire.test.ak`. Constructor order *is* the wire here: a
 * reordered `State` would silently reinterpret a live thread's checkpoint, and
 * a reordered `ActionV1` would turn an `Advance` into a `FinishGrammar`.
 */
describe("Q35 network-id forced-scan wire twins", () => {
  const fraudProver = "11".repeat(28);
  const badTxId = "22".repeat(32);
  const orderKeyTransactionId = "77".repeat(32);
  const forcedSourceKey = `d8799f5820${orderKeyTransactionId}00ff`;
  const checkpointHash = "88".repeat(32);
  const checkpointBytes = "9999";

  const bound = {
    bad_tx_id: badTxId,
    committed_tx_network_id: 255n,
    expected_network_id: 0n,
    forced_source_key: forcedSourceKey,
  } as const;

  /** The smallest tier-1 opening; every action carries the same one. */
  const opening = {
    BodyFieldOpening: {
      native_tx_compact_cbor: "80",
      carriage: { Inline: { preimage: "81" } },
    },
  } as const;
  const indices = { input_index: 0n, output_index: 0n } as const;

  const FORCED_SCAN_BOUND_CBOR =
    "d8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff";
  const FORCED_SCAN_READY_STATE_CBOR =
    "d8799fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffffff";
  const FORCED_SCAN_GRAMMAR_STATE_CBOR =
    "d87a9fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff58208888888888888888888888888888888888888888888888888888888888888888ff";
  const FORCED_SCAN_SCANNING_STATE_CBOR =
    "d87b9fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff58208888888888888888888888888888888888888888888888888888888888888888ff";
  const FORCED_SCAN_DATUM_CBOR =
    "d8799f581c11111111111111111111111111111111111111111111111111111111d8799fd87b9fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff58208888888888888888888888888888888888888888888888888888888888888888ffffff";
  const EMPTY_FORCED_SCAN_DATUM_CBOR =
    "d8799f581c11111111111111111111111111111111111111111111111111111111d87a80ff";
  const FORCED_SCAN_OPEN_ACTION_CBOR = "d8799f0000d8799f4180d8799f4181ffffff";
  const FORCED_SCAN_START_GRAMMAR_ACTION_CBOR =
    "d87a9f0000d8799f4180d8799f4181ffff1880ff";
  const FORCED_SCAN_RESUME_GRAMMAR_ACTION_CBOR =
    "d87b9f0000d8799f4180d8799f4181ffff4299991880ff";
  const FORCED_SCAN_FINISH_GRAMMAR_ACTION_CBOR =
    "d87c9f0000d8799f4180d8799f4181ffff429999ff";
  const FORCED_SCAN_ADVANCE_ACTION_CBOR =
    "d87d9f0000d8799f4180d8799f4181ffff4299991840ff";
  const FORCED_SCAN_REDEEMER_CBOR =
    "d87a9fd87d9f0000d8799f4180d8799f4181ffff4299991840ffff";
  const FORCED_SCAN_CANCEL_REDEEMER_CBOR = "d8799f0000ff";

  it("pins the bound the forced door freezes into the scan", () => {
    expect(Data.to(bound as never, NetworkIdForcedScanBoundSchema)).toBe(
      FORCED_SCAN_BOUND_CBOR,
    );
  });

  it("pins the three thread states in constructor order", () => {
    expect(
      Data.to({ Ready: { bound } } as never, NetworkIdForcedScanStateSchema),
    ).toBe(FORCED_SCAN_READY_STATE_CBOR);
    expect(
      Data.to(
        { Grammar: { bound, checkpoint_hash: checkpointHash } } as never,
        NetworkIdForcedScanStateSchema,
      ),
    ).toBe(FORCED_SCAN_GRAMMAR_STATE_CBOR);
    expect(
      Data.to(
        { Scanning: { bound, checkpoint_hash: checkpointHash } } as never,
        NetworkIdForcedScanStateSchema,
      ),
    ).toBe(FORCED_SCAN_SCANNING_STATE_CBOR);
  });

  it("pins the scan datum and the empty state every action refuses", () => {
    expect(
      Data.to(
        {
          fraud_prover: fraudProver,
          data: { Scanning: { bound, checkpoint_hash: checkpointHash } },
        } as never,
        NetworkIdForcedScanDatumSchema,
      ),
    ).toBe(FORCED_SCAN_DATUM_CBOR);
    expect(
      Data.to(
        { fraud_prover: fraudProver, data: null } as never,
        NetworkIdForcedScanDatumSchema,
      ),
    ).toBe(EMPTY_FORCED_SCAN_DATUM_CBOR);
  });

  it("pins every action in constructor order", () => {
    expect(
      Data.to(
        { Open: { ...indices, opening } } as never,
        NetworkIdForcedScanActionSchema,
      ),
    ).toBe(FORCED_SCAN_OPEN_ACTION_CBOR);
    expect(
      Data.to(
        {
          StartGrammar: {
            ...indices,
            opening,
            item_budget: NETWORK_ID_FORCED_GRAMMAR_BATCH,
          },
        } as never,
        NetworkIdForcedScanActionSchema,
      ),
    ).toBe(FORCED_SCAN_START_GRAMMAR_ACTION_CBOR);
    expect(
      Data.to(
        {
          ResumeGrammar: {
            ...indices,
            opening,
            checkpoint_bytes: checkpointBytes,
            item_budget: NETWORK_ID_FORCED_GRAMMAR_BATCH,
          },
        } as never,
        NetworkIdForcedScanActionSchema,
      ),
    ).toBe(FORCED_SCAN_RESUME_GRAMMAR_ACTION_CBOR);
    expect(
      Data.to(
        {
          FinishGrammar: {
            ...indices,
            opening,
            checkpoint_bytes: checkpointBytes,
          },
        } as never,
        NetworkIdForcedScanActionSchema,
      ),
    ).toBe(FORCED_SCAN_FINISH_GRAMMAR_ACTION_CBOR);
    expect(
      Data.to(
        {
          Advance: {
            ...indices,
            opening,
            checkpoint_bytes: checkpointBytes,
            item_budget: NETWORK_ID_FORCED_SCAN_BATCH,
          },
        } as never,
        NetworkIdForcedScanActionSchema,
      ),
    ).toBe(FORCED_SCAN_ADVANCE_ACTION_CBOR);
  });

  it("pins the scan spend redeemer in both directions", () => {
    expect(
      Data.to(
        {
          Continue: [
            {
              Advance: {
                ...indices,
                opening,
                checkpoint_bytes: checkpointBytes,
                item_budget: NETWORK_ID_FORCED_SCAN_BATCH,
              },
            },
          ],
        } as never,
        NetworkIdForcedScanSpendRedeemerSchema,
      ),
    ).toBe(FORCED_SCAN_REDEEMER_CBOR);
    expect(
      Data.to(
        {
          Cancel: {
            input_index: 0n,
            computation_thread_mint_redeemer_index: 0n,
          },
        } as never,
        NetworkIdForcedScanSpendRedeemerSchema,
      ),
    ).toBe(FORCED_SCAN_CANCEL_REDEEMER_CBOR);
  });

  it("pins the batch caps the builder never proposes above", () => {
    expect(NETWORK_ID_FORCED_SCAN_BATCH).toBe(64n);
    expect(NETWORK_ID_FORCED_GRAMMAR_BATCH).toBe(128n);
  });
});
