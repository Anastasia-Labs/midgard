import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { DepositEvent, WithdrawalEvent } from "@al-ft/midgard-sdk";
import {
  canonicalCommittedWithdrawalTransitionEffect,
  type CanonicalTransitionEffect,
  deriveCanonicalOriginalDepositTransitionEffect,
} from "@al-ft/midgard-validation";
import type {
  PhaseAValidatedTx,
  PhaseBConfig,
} from "@al-ft/midgard-validation/types";
import { Data as LucidData } from "@lucid-evolution/lucid";

import {
  canonicalEffectFromAuthority,
  decodeUserEventIdCborHex,
  eventIdForKey,
  eventIdForKeyCborHex,
  eventKeyFingerprint,
  ledgerOutRefCborHex,
  sha256Hex,
  userEventKindForPhase,
  type ValidatedEventAuthority,
  watcherBlockReplayEventAuthorityManifest,
  type WatcherBlockReplayEventOriginRecord,
} from "./block-replay.watcher-block-replay-prior-state.js";
import {
  type WatcherBlockReplayContext,
  type WatcherBlockReplayPriorUtxo,
} from "./block-replay.watcher-block-replay-rejection-projection.js";
import {
  fail,
  type WatcherBlockReplayEventAuthority,
} from "./block-replay.watcher-block-replay-result.js";
import {
  bindWatcherOriginEventClaim,
  type WatcherCommittedEventClaim,
} from "./event-claims.js";
import { type WatcherRuleBundle } from "./rule-bundle.js";
import {
  readWatcherUserEventAuthority,
  watcherOriginalDepositAssets,
  type WatcherUserEvent,
  type WatcherUserEventAuthority,
  type WatcherUserEventHeaderCutoff,
} from "./user-event.js";

export const eventEffectManifest = (
  phase: WatcherBlockReplayEventAuthority["phase"],
  fingerprint: string,
  effect: CanonicalTransitionEffect,
): Readonly<Record<string, unknown>> =>
  Object.freeze({
    phase,
    eventKeyFingerprint: fingerprint,
    effectDigest: effect.digest,
    effectCborSha256: sha256Hex(effect.canonicalCbor),
    operations: effect.operations.map((operation) => ({
      type: operation.type,
      outRefCbor: operation.outRefCbor.toString("hex"),
      ...(operation.type === "insert"
        ? { outputCborSha256: sha256Hex(operation.outputCbor) }
        : {}),
    })),
  });

export type ReplayDeploymentBinding = Readonly<
  Pick<WatcherRuleBundle, "deploymentManifestId" | "blueprintHash" | "network">
>;

export type ReplayHeaderBinding = Pick<
  WatcherUserEventHeaderCutoff,
  "headerHash" | "headerCborHex" | "observedBlockHash" | "observedSlot"
>;

export const readUserEventAuthority = async (
  receipt: WatcherUserEventAuthority,
) => {
  try {
    return await readWatcherUserEventAuthority(receipt);
  } catch {
    return fail("user_event_authority_invalid", "$.userEvent");
  }
};

export const validateEventAuthority = async (
  authority: WatcherBlockReplayEventAuthority,
  claims: readonly WatcherCommittedEventClaim[],
  deployment: ReplayDeploymentBinding,
  header: ReplayHeaderBinding,
): Promise<ValidatedEventAuthority> => {
  const eventId = eventIdForKeyCborHex(authority.eventKey);
  const eventOutRef = eventIdForKey(authority.eventKey);
  const expectedKind = userEventKindForPhase(authority.phase);
  const local = await readUserEventAuthority(authority.userEvent);
  if (
    local.deploymentManifestId !== deployment.deploymentManifestId ||
    local.blueprintHash !== deployment.blueprintHash ||
    local.network !== deployment.network
  ) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.deployment",
    );
  }
  if (
    local.throughHeader.headerHash !== header.headerHash ||
    local.throughHeader.headerCborHex !== header.headerCborHex ||
    local.throughHeader.observedBlockHash !== header.observedBlockHash ||
    local.throughHeader.observedSlot !== header.observedSlot
  ) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.throughHeader",
    );
  }
  const event: WatcherUserEvent = local.event;
  const network: WatcherRuleBundle["network"] = local.network;
  const origin: WatcherBlockReplayEventOriginRecord = Object.freeze({
    source: "follower_facts",
    deploymentManifestId: local.deploymentManifestId,
    blueprintHash: local.blueprintHash,
    throughHeader: local.throughHeader,
  });
  if (
    event.kind !== expectedKind ||
    event.eventId !== eventId ||
    decodeUserEventIdCborHex(event) !== eventId
  ) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.eventAuthority.eventId",
    );
  }
  if (
    event.nonceOutRef !==
    `${eventOutRef.transactionId}#${eventOutRef.outputIndex.toString()}`
  ) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.event.nonceOutRef",
    );
  }
  const matchesClaim = claims.filter(
    (claim) =>
      claim.phase === authority.phase && claim.eventIdCborHex === event.eventId,
  );
  if (matchesClaim.length !== 1)
    return fail("event_authority_identity_mismatch", "$.committedEventClaim");
  const claim = matchesClaim[0]!;
  let bound;
  try {
    bound = bindWatcherOriginEventClaim(event, claim);
  } catch {
    return fail("event_authority_identity_mismatch", "$.committedEventClaim");
  }
  let effect: CanonicalTransitionEffect | null = null;
  let canonicalNativeTxCbor: Buffer | null = null;
  let programMaterialSidecarCbor: Buffer | null = null;
  const committedForcedValidity =
    bound.phase === "ForcedTransaction" ? bound.operatorValidity : null;
  if (authority.phase === "Withdrawal") {
    effect = canonicalEffectFromAuthority(authority);
    if (bound.phase !== "Withdrawal")
      return fail(
        "event_authority_identity_mismatch",
        "$.committedEventClaim.phase",
      );
    const isCommittedValid = bound.committed.validity === "WithdrawalIsValid";
    const decoded = LucidData.from(
      event.eventCborHex,
      WithdrawalEvent as never,
    ) as {
      readonly info: {
        readonly body: {
          readonly l2_outref: {
            readonly transactionId: string;
            readonly outputIndex: bigint;
          };
        };
      };
    };
    const expectedOutRef = ledgerOutRefCborHex(decoded.info.body.l2_outref);
    const derivedEffect = canonicalCommittedWithdrawalTransitionEffect({
      committedValid: isCommittedValid,
      outRefCbor: Buffer.from(expectedOutRef, "hex"),
    });
    if (
      effect.digest !== derivedEffect.digest ||
      !effect.canonicalCbor.equals(derivedEffect.canonicalCbor)
    ) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.transitionEffect.operations",
      );
    }
  } else if (authority.phase === "Deposit") {
    effect = canonicalEffectFromAuthority(authority);
    const decoded = LucidData.from(
      event.eventCborHex,
      DepositEvent as never,
    ) as {
      readonly id: {
        readonly transactionId: string;
        readonly outputIndex: bigint;
      };
      readonly info: {
        readonly l2_network_id: bigint;
        readonly l2_address: Parameters<
          typeof deriveCanonicalOriginalDepositTransitionEffect
        >[0]["l2Address"];
        readonly l2_datum: unknown | null;
      };
    };
    const derivedEffect = deriveCanonicalOriginalDepositTransitionEffect({
      configuredNetwork: network,
      eventId: decoded.id,
      l2NetworkId: decoded.info.l2_network_id,
      l2Address: decoded.info.l2_address,
      l2DatumCbor:
        decoded.info.l2_datum === null
          ? null
          : Buffer.from(
              plutusConstrFieldCbor(event.eventCborHex, [1, 2, 0]),
              "hex",
            ),
      originalAssets: watcherOriginalDepositAssets(event),
    });
    if (
      effect.operations.length !== 1 ||
      effect.operations[0]!.type !== "insert" ||
      effect.operations[0]!.outRefCbor.toString("hex") !==
        ledgerOutRefCborHex(decoded.id) ||
      effect.digest !== derivedEffect.digest ||
      !effect.canonicalCbor.equals(derivedEffect.canonicalCbor)
    ) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.transitionEffect.operations",
      );
    }
  } else {
    if ("transitionEffect" in authority) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.transitionEffect.forced.callerEffect",
      );
    }
    if (
      authority.canonicalNativeTxCbor === undefined ||
      authority.canonicalNativeTxCbor === null
    ) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.canonicalNativeTxCbor",
      );
    }
    canonicalNativeTxCbor = Buffer.from(authority.canonicalNativeTxCbor);
    programMaterialSidecarCbor =
      authority.programMaterialSidecarCbor === undefined ||
      authority.programMaterialSidecarCbor === null
        ? null
        : Buffer.from(authority.programMaterialSidecarCbor);
    if (
      canonicalNativeTxCbor.toString("hex") !== claim.canonicalNativeTxCborHex
    ) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.canonicalNativeTxCbor.binding",
      );
    }
  }
  const recordSource = Object.freeze({
    phase: authority.phase,
    eventKey: authority.eventKey,
    event,
    network,
    origin,
    committedClaim: claim,
    canonicalNativeTxCborHex: canonicalNativeTxCbor?.toString("hex") ?? null,
    programMaterialSidecarCborHex:
      programMaterialSidecarCbor?.toString("hex") ?? null,
  });
  const authorityManifest =
    watcherBlockReplayEventAuthorityManifest(recordSource);
  const effectManifest =
    effect === null
      ? null
      : eventEffectManifest(
          authority.phase,
          eventKeyFingerprint(authority.eventKey),
          effect,
        );
  return Object.freeze({
    phase: authority.phase,
    eventKeyFingerprint: eventKeyFingerprint(authority.eventKey),
    effect,
    canonicalNativeTxCbor,
    programMaterialSidecarCbor,
    committedForcedValidity,
    userEvent: event,
    authorityManifest,
    effectManifest,
    recordSource,
  });
};

export type EvaluateWatcherBlockReplayCandidatesInput = {
  /** Canonical Phase A candidates, in canonical block order. */
  readonly candidates: readonly PhaseAValidatedTx[];
  /** Prior-state ledger entries, from the W21 records. */
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  /** The L1-committed `prevUtxosRoot` the prior state must reproduce. */
  readonly expectedPriorStateRoot: string;
  /** The L1-committed `utxosRoot`, or null to leave the post state unbound. */
  readonly expectedPostStateRoot?: string | null;
  readonly config: PhaseBConfig;
  readonly context?: WatcherBlockReplayContext;
};
