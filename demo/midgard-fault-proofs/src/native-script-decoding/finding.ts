/**
 * The `native-script-decoding` finding record (offchain plan §3.4): the
 * typed CONTRACT between detection and proving. It is the sole input the
 * proving core (§4.3) accepts, and it is deliberately self-contained —
 * everything needed to start (or resume) a thread is derivable from it
 * plus chain state, so the same record drives the watcher's autonomous
 * path and an operator's manual CLI invocation alike.
 */
import {
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL,
} from "@al-ft/midgard-sdk";

import { nativeScriptDecodingSubmitError } from "./submit-common.js";

/**
 * The §3.2/3.3 provability classification. The first three are the
 * provable routes; the last two are refused at the proving core's API
 * boundary regardless of policy — the classification, not the consumer,
 * is the gate (§3.4).
 */
export const NativeScriptDecodingProvability = Object.freeze({
  /** Direction B, descriptor language ≠ 0: provable at bind (§3.2). */
  DescriptorContradiction: "descriptorContradiction",
  /**
   * A tag-0 descriptor whose scan runs to a terminal: direction B's exact
   * canonical terminal, or direction A's refusing step (§3.2/3.3).
   */
  MachineRoute: "machineRoute",
  /** Direction B accusing a pair outside the committed domain (§7.2). */
  OutOfDomainAccusation: "outOfDomainAccusation",
  /**
   * The §7.3 corner: descriptor tag 0 but the wrapper decodes non-native.
   * Journaled, never routed to proving — step-03 fails it in both
   * directions by design.
   */
  WrapperContradiction: "wrapperContradiction",
  /**
   * The rejection is substantively right (or the acceptance sound); class
   * misattribution is not provable by this family (design §7.6 residual).
   */
  NotAFault: "notAFault",
} as const);
export type NativeScriptDecodingProvability =
  (typeof NativeScriptDecodingProvability)[keyof typeof NativeScriptDecodingProvability];

/** The provability classes the proving core accepts. */
export const NATIVE_SCRIPT_DECODING_PROVABLE_CLASSES: readonly NativeScriptDecodingProvability[] =
  [
    NativeScriptDecodingProvability.DescriptorContradiction,
    NativeScriptDecodingProvability.MachineRoute,
    NativeScriptDecodingProvability.OutOfDomainAccusation,
  ];

/** The faulted event: a committed L2 transaction, or a forced event's order key. */
export type NativeScriptDecodingFindingEvent =
  | {
      readonly kind: "l2Transaction";
      /** 32-byte transaction id, lowercase hex. */
      readonly txId: string;
    }
  | {
      readonly kind: "forcedEvent";
      /** The forced event's `TxOrderId`, serialised as canonical CBOR hex. */
      readonly orderKeyCbor: string;
    };

/**
 * The descriptor fields the detector already resolved (§3.4). `null` for
 * the out-of-domain route, where no descriptor exists to resolve.
 */
export type NativeScriptDecodingFindingDescriptor = {
  /** The descriptor's reference-script language tag (0 = native). */
  readonly referenceScriptLanguage: number;
  /** The output index the descriptor commits. */
  readonly outputIndex: number;
  /** The reference-script item's committed total length in bytes. */
  readonly totalLength: number;
};

export type NativeScriptDecodingFinding = {
  /** 0 = wrongful acceptance (direction A), 1 = wrongful rejection (B). */
  readonly direction: bigint;
  /** 0 = normal L2 transaction, 1 = forced event. */
  readonly sourceKind: bigint;
  readonly event: NativeScriptDecodingFindingEvent;
  /** The faulted block's 28-byte header hash, lowercase hex. */
  readonly headerHash: string;
  /** `txHash#index` of the faulted block's state-queue UTxO. */
  readonly fraudulentBlockOutRef: string;
  /** The accused (direction B) or prover-chosen (direction A) pair. */
  readonly accusedOutpointSourceKind: bigint;
  readonly accusedOutpointCursor: bigint;
  /** Direction B: the accused scan-reason class {0, 1, 2}. Direction A: null. */
  readonly scanReasonClass: bigint | null;
  readonly provability: NativeScriptDecodingProvability;
  readonly descriptor: NativeScriptDecodingFindingDescriptor | null;
  /** §6 plan-time estimate of the thread's total L1 transaction count. */
  readonly estimatedThreadTxCount: number;
};

/**
 * The §3.2/3.3 boundary gate plus the record's own structural coherence.
 * Refusals here are classification refusals — no policy can override them.
 */
export const assertNativeScriptDecodingFindingProvable = (
  finding: NativeScriptDecodingFinding,
): void => {
  if (!NATIVE_SCRIPT_DECODING_PROVABLE_CLASSES.includes(finding.provability)) {
    throw nativeScriptDecodingSubmitError(
      `finding class "${finding.provability}" is not provable by this family (§3.2/3.3) — it is journaled, never proven.`,
    );
  }
  const isDirectionB =
    finding.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION;
  if (
    !isDirectionB &&
    finding.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE
  ) {
    throw nativeScriptDecodingSubmitError(
      `finding direction ${finding.direction.toString()} is outside {0, 1}.`,
    );
  }
  if (
    finding.sourceKind !== NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL &&
    finding.sourceKind !== NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED
  ) {
    throw nativeScriptDecodingSubmitError(
      `finding source kind ${finding.sourceKind.toString()} is outside {0, 1}.`,
    );
  }
  if (isDirectionB) {
    // Only a forced leaf carries an explicit rejection to dispute.
    if (finding.sourceKind !== NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED) {
      throw nativeScriptDecodingSubmitError(
        "a wrongful-rejection finding must name a forced source.",
      );
    }
    if (finding.event.kind !== "forcedEvent") {
      throw nativeScriptDecodingSubmitError(
        "a forced-source finding must carry the forced event's order key.",
      );
    }
    if (finding.scanReasonClass === null) {
      throw nativeScriptDecodingSubmitError(
        "a wrongful-rejection finding must carry the accused scan-reason class.",
      );
    }
  } else {
    if (
      finding.provability ===
      NativeScriptDecodingProvability.OutOfDomainAccusation
    ) {
      throw nativeScriptDecodingSubmitError(
        "the out-of-domain close is direction B's alone (§7.2).",
      );
    }
    if (
      finding.sourceKind === NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL &&
      finding.event.kind !== "l2Transaction"
    ) {
      throw nativeScriptDecodingSubmitError(
        "a normal-source finding must carry the committed transaction id.",
      );
    }
  }
  if (
    finding.provability !==
      NativeScriptDecodingProvability.OutOfDomainAccusation &&
    finding.descriptor === null
  ) {
    throw nativeScriptDecodingSubmitError(
      "a bindable finding must carry the resolved descriptor fields.",
    );
  }
  if (
    finding.provability ===
      NativeScriptDecodingProvability.DescriptorContradiction &&
    finding.descriptor?.referenceScriptLanguage === 0
  ) {
    throw nativeScriptDecodingSubmitError(
      "a descriptor-contradiction finding cannot name a tag-0 descriptor — that is the machine route.",
    );
  }
  if (
    finding.provability === NativeScriptDecodingProvability.MachineRoute &&
    finding.descriptor !== null &&
    finding.descriptor.referenceScriptLanguage !== 0
  ) {
    throw nativeScriptDecodingSubmitError(
      "the machine route scans tag-0 descriptors only.",
    );
  }
};
