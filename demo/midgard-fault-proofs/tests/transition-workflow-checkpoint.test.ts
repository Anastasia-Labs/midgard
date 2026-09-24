import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { transitionDepositCheckpointRequiresQueueLease } from "../src/transition-trace/workflow-checkpoint.js";

const address = "deposit-final-address";
const unit = "ab".repeat(28) + "00000001" + "cd".repeat(28);
const prover = "ee".repeat(28);
const proofHash = "ff".repeat(32);
const state = (phase: bigint): SDK.TransitionTraceFinalState => ({
  kind: 1n,
  phase,
  proof_commitment: { hash: proofHash },
  opened: { spend_input_keys: [], output_hashes: [] },
  input_index: 0n,
  output_index: 0n,
  current_root: "",
  summaries: { summaries: [] },
  scan_cbor: "",
  value_cbor: "",
  value_start: 0n,
  value_end: 0n,
  value_summary: null,
  descriptor_cbor: "",
  deposit_index: 0n,
  deposit_source_cbor: "",
  deposit_asset_count: 0n,
});
const thread = (phase = 5n): UTxO => ({
  address,
  txHash: "aa".repeat(32),
  outputIndex: 0,
  assets: { lovelace: 3_000_000n, [unit]: 1n },
  datum: Data.to(
    { fraud_prover: prover, data: state(phase) },
    SDK.TransitionTraceProofCommitmentDatum,
  ),
});
const required = (utxo: UTxO) =>
  transitionDepositCheckpointRequiresQueueLease({
    thread: utxo,
    address,
    unit,
    prover,
    proofHash,
  });

describe("transition deposit queue mutation checkpoint", () => {
  it("requires a lease only for terminal phase five", () => {
    for (const phase of [6n, 0n, 10n, 2n, 7n, 8n, 3n, 9n, 4n])
      expect(required(thread(phase))).toBe(false);
    expect(required(thread())).toBe(true);
  });
  it.each([
    ["address", (t: UTxO) => ({ ...t, address: "another-address" })],
    [
      "missing token",
      (t: UTxO) => ({ ...t, assets: { lovelace: 3_000_000n } }),
    ],
    [
      "duplicate token",
      (t: UTxO) => ({ ...t, assets: { ...t.assets, [unit]: 2n } }),
    ],
    [
      "foreign token",
      (t: UTxO) => ({ ...t, assets: { ...t.assets, ["bb".repeat(28)]: 1n } }),
    ],
    ["missing datum", (t: UTxO) => ({ ...t, datum: undefined })],
    [
      "script reference",
      (t: UTxO) => ({
        ...t,
        scriptRef: { type: "PlutusV3" as const, script: "00" },
      }),
    ],
  ] as const)("refuses changed %s", (_label, change) =>
    expect(() => required(change(thread()))).toThrow(/authentication/),
  );
  it.each([
    ["owner", { fraud_prover: "ab".repeat(28), data: state(5n) }],
    [
      "proof",
      {
        fraud_prover: prover,
        data: { ...state(5n), proof_commitment: { hash: "00".repeat(32) } },
      },
    ],
    ["kind", { fraud_prover: prover, data: { ...state(5n), kind: 0n } }],
    ["state", { fraud_prover: prover, data: null }],
  ] as const)("refuses changed %s", (_label, datum) =>
    expect(() =>
      required({
        ...thread(),
        datum: Data.to(datum, SDK.TransitionTraceProofCommitmentDatum),
      }),
    ).toThrow(/admitted proof/),
  );
});
