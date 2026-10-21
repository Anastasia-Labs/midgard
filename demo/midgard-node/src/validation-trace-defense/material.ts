/**
 * A defending operator's material for a case on one of its landed blocks:
 * the block's evidence from the node's retained DA payload, checked
 * against the header the landed queue names, and its predecessor ledger
 * from the landed ledger at the block's parent, checked against the
 * parent's root; then the defense material the fault-proofs package builds
 * from them (our trace of the case's event at the case's step count).
 *
 * Every transient lack (no retained payload, a parent off the landed
 * ledger, a ledger that does not open its root, a build that throws) is
 * `unavailable` with its detail; the tick retries it. A deterministic
 * outcome (our own fault, or a trace that disagrees) is final for the case.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  buildValidationTraceDefenseMaterial,
  canonicalBlockEvidenceFromVerifiedPayload,
  checkLedgerAgainstRoot,
  type ValidationTraceDefenseMaterial,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { decodeStoredPayload } from "../da/foreign-retained-da.js";
import { DaPayloadsDB } from "../database/index.js";
import { type LandedLedger, ledgerAt } from "../landed-blocks/ledger.js";
import type { LandedBlockRow } from "../landed-blocks/store.js";

export type ValidationTraceDefenseOutcome =
  | Readonly<{
      kind: "defend";
      material: Extract<ValidationTraceDefenseMaterial, { kind: "defend" }>;
    }>
  | Readonly<{ kind: "operator_fault"; detail: string }>
  | Readonly<{ kind: "trace_disagrees"; detail: string }>
  | Readonly<{ kind: "unavailable"; detail: string }>;

/** Where the node observed the landed block: the follower's cursor. */
export type DefenseObservationPoint = Readonly<{ slot: number; hash: string }>;

const unavailable = (detail: string): ValidationTraceDefenseOutcome => ({
  kind: "unavailable",
  detail,
});

/**
 * The defense material for a case on landed block `target`, on the case's
 * event at its step count, given `landed` (the landed ledger, which holds
 * `target`'s parent while `target` is unmerged).
 */
export const validationTraceDefenseOutcome = (
  input: Readonly<{
    target: LandedBlockRow;
    landed: LandedLedger;
    caseDatum: SDK.Case;
    point: DefenseObservationPoint;
    l1SlotConfig: L1SlotConfig;
  }>,
) =>
  Effect.gen(function* () {
    const { target, caseDatum } = input;
    const stored = yield* DaPayloadsDB.retrieveByHeaderHash(
      Buffer.from(target.headerHash, "hex"),
    );
    if (Option.isNone(stored))
      return unavailable(
        `no retained DA payload of block ${target.headerHash}`,
      );
    const parent = ledgerAt(input.landed, target.parentHeaderHash);
    if (parent === undefined)
      return unavailable(
        `the parent ${target.parentHeaderHash} of block ${target.headerHash} is not on the landed ledger`,
      );
    const payloadEnvelopeCbor = stored.value[DaPayloadsDB.Columns.PAYLOAD_CBOR];
    return yield* Effect.promise(async () => {
      try {
        const payload = await decodeStoredPayload({
          payloadCbor: payloadEnvelopeCbor,
          schemaVersion: stored.value[DaPayloadsDB.Columns.VERSION],
        });
        const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
          observation: {
            schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
            sourceMode: "local_node",
            provenance: {
              trustClass: "authenticated_cardano_l1",
              sourceId: "node-l1-follower",
              grade: "security",
            },
            chainPoint: {
              slot: BigInt(input.point.slot),
              blockHash: input.point.hash,
            },
            confirmationDepth: 1,
            headerHash: target.headerHash,
            header: payload.block_body.header,
          },
          payloadEnvelopeCbor,
          daProvenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "node-retained-da",
            grade: "security",
          },
        });
        const predecessor = await checkLedgerAgainstRoot({
          entries: [...parent.ledger].map(([outRef, output]) => ({
            outRef: Buffer.from(outRef, "hex"),
            output,
          })),
          utxosRoot: target.parentUtxosRoot,
          label: `the landed ledger at ${target.parentHeaderHash}`,
        });
        const material = await buildValidationTraceDefenseMaterial({
          evidence,
          predecessor,
          eventKey: caseDatum.event_key,
          stepCount: Number(caseDatum.step_count),
          l1SlotConfig: input.l1SlotConfig,
        });
        switch (material.kind) {
          case "defend":
            return { kind: "defend", material } as const;
          case "operator_fault":
            return {
              kind: "operator_fault",
              detail: `the replay rejects the event the block accepted${material.rejection === undefined ? "" : `: ${material.rejection.code}`}`,
            } as const;
          case "trace_disagrees":
            return {
              kind: "trace_disagrees",
              detail: material.detail,
            } as const;
        }
      } catch (cause) {
        return unavailable(formatUnknownError(cause));
      }
    });
  });
