import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  Data,
  mintingPolicyToId,
  type Network,
  type SpendingValidator,
  validatorToAddress,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  AddressSchema,
  OutputReference,
  OutputReferenceSchema,
  ProofSchema,
  ValueSchema,
} from "../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
} from "../fraud-proof/contracts/blueprint.js";
import {
  CardanoDatumSchema,
  DepositEventSchema,
  WithdrawalEventSchema,
  WithdrawalValiditySchema,
} from "../ledger-state.js";
import {
  applyEventHistoryRetentionValidator,
  EventHistoryKind,
  EventHistoryKindSchema,
} from "./history-data.js";

const keySchema = Data.Bytes({ minLength: 32, maxLength: 32 });
const signerSchema = Data.Bytes({ minLength: 28, maxLength: 28 });
export const EVENT_HISTORY_MAX_PROTECTION_TIME = 9_223_372_036_854_775_807n;
export const EVENT_HISTORY_CONTRACT_TITLES = Object.freeze({
  spend: "user_events/history.history.spend",
  mint: "user_events/history.history.mint",
  withdraw: "user_events/history.history.withdraw",
  retirement: "user_events/history_retirement.retirement_observer.withdraw",
});

export const EventHistoryPayloadSchema = Data.Enum([
  Data.Object({ DepositPayload: Data.Object({ event: DepositEventSchema }) }),
  Data.Object({
    WithdrawalPayload: Data.Object({
      event: WithdrawalEventSchema,
      refund_address: AddressSchema,
      refund_datum: CardanoDatumSchema,
    }),
  }),
]);
export type EventHistoryPayload = Data.Static<typeof EventHistoryPayloadSchema>;
export const EventHistoryPayload = asDataType<EventHistoryPayload>(
  EventHistoryPayloadSchema,
);

export const EventHistoryFactsSchema = Data.Object({
  event_id: OutputReferenceSchema,
  inclusion_time: Data.Integer({ minimum: 0 }),
  location: Data.Enum([
    Data.Object({
      Inline: Data.Object({ payload: EventHistoryPayloadSchema }),
    }),
    Data.Object({ External: Data.Object({ storage_datum_hash: keySchema }) }),
  ]),
  structural_lovelace: Data.Integer({ minimum: 0 }),
  structural_refund_key: signerSchema,
});
export type EventHistoryFacts = Data.Static<typeof EventHistoryFactsSchema>;
export const EventHistoryFacts = asDataType<EventHistoryFacts>(
  EventHistoryFactsSchema,
);

/** Immutable facts captured from an authenticated Order by a proof stage. */
export const EventHistoryCommitmentSchema = Data.Object({
  policy: signerSchema,
  kind: EventHistoryKindSchema,
  event_id: OutputReferenceSchema,
  inclusion_time: Data.Integer(),
  payload_hash: keySchema,
  original_assets_hash: keySchema,
});
export type EventHistoryCommitment = Data.Static<
  typeof EventHistoryCommitmentSchema
>;
export const EventHistoryCommitment = asDataType<EventHistoryCommitment>(
  EventHistoryCommitmentSchema,
);

/** Preimage reopened against a commitment already authenticated on L1. */
export const EventHistoryOpeningSchema = Data.Object({
  payload: EventHistoryPayloadSchema,
  original_assets: ValueSchema,
});
export type EventHistoryOpening = Data.Static<typeof EventHistoryOpeningSchema>;
export const EventHistoryOpening = asDataType<EventHistoryOpening>(
  EventHistoryOpeningSchema,
);

export const EventHistoryNodeSchema = Data.Object({
  position: Data.Enum([
    Data.Literal("Root"),
    Data.Object({ Key: Data.Tuple([keySchema]) }),
  ]),
  next: Data.Nullable(keySchema),
  protected_until: Data.Integer({ minimum: 0 }),
  payload: Data.Enum([
    Data.Literal("RootContent"),
    Data.Object({ Filler: Data.Object({ refund_key: signerSchema }) }),
    Data.Object({ Order: Data.Object({ facts: EventHistoryFactsSchema }) }),
  ]),
});
export type EventHistoryNode = Data.Static<typeof EventHistoryNodeSchema>;
export const EventHistoryNode = asDataType<EventHistoryNode>(
  EventHistoryNodeSchema,
);

export const EventHistoryRetirementWitnessSchema = Data.Object({
  predecessor_input_index: Data.Integer(),
  order_input_index: Data.Integer(),
  predecessor_output_index: Data.Integer(),
  funds_output_index: Data.Integer(),
  structural_refund_output_index: Data.Nullable(Data.Integer()),
  confirmed_reference_index: Data.Integer(),
  settlement_reference_index: Data.Integer(),
  external_reference_index: Data.Nullable(Data.Integer()),
  membership: Data.Object({
    phas_root: keySchema,
    count: Data.Integer({ minimum: 1 }),
    proof: ProofSchema,
  }),
  purpose: Data.Enum([
    Data.Literal("AbsorbDeposit"),
    Data.Literal("InitializeWithdrawalPayout"),
    Data.Object({
      RefundInvalidWithdrawal: Data.Object({
        validity: WithdrawalValiditySchema,
      }),
    }),
  ]),
});
export type EventHistoryRetirementWitness = Data.Static<
  typeof EventHistoryRetirementWitnessSchema
>;
export const EventHistoryRetirementWitness =
  asDataType<EventHistoryRetirementWitness>(
    EventHistoryRetirementWitnessSchema,
  );

/** Exact redeemer of the deployment's zero-withdrawal retirement observer. */
export const EventHistoryRetirementArgsSchema = Data.Object({
  hub_reference_index: Data.Integer(),
  witness: EventHistoryRetirementWitnessSchema,
});
export type EventHistoryRetirementArgs = Data.Static<
  typeof EventHistoryRetirementArgsSchema
>;
export const EventHistoryRetirementArgs =
  asDataType<EventHistoryRetirementArgs>(EventHistoryRetirementArgsSchema);

/** Output claims are derived from these operations by both list observers. */
export const EventHistoryOperationSchema = Data.Enum([
  Data.Object({
    InsertFiller: Data.Object({
      predecessor_input_index: Data.Integer(),
      predecessor_output_index: Data.Integer(),
      filler_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    InsertOrder: Data.Object({
      predecessor_input_index: Data.Integer(),
      predecessor_output_index: Data.Integer(),
      order_output_index: Data.Integer(),
      nonce_input_index: Data.Integer(),
      external_reference_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    PromoteFiller: Data.Object({
      filler_input_index: Data.Integer(),
      order_output_index: Data.Integer(),
      refund_output_index: Data.Integer(),
      nonce_input_index: Data.Integer(),
      external_reference_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    ReclaimFiller: Data.Object({
      predecessor_input_index: Data.Integer(),
      filler_input_index: Data.Integer(),
      predecessor_output_index: Data.Integer(),
      refund_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RetireOrder: Data.Object({
      predecessor_output_index: Data.Integer(),
      funds_output_index: Data.Integer(),
      structural_refund_output_index: Data.Nullable(Data.Integer()),
    }),
  }),
]);
export type EventHistoryOperation = Data.Static<
  typeof EventHistoryOperationSchema
>;
export const EventHistoryOperation = asDataType<EventHistoryOperation>(
  EventHistoryOperationSchema,
);
export const EventHistoryObserveSchema = Data.Enum([
  Data.Object({
    Initialize: Data.Object({
      nonce_input_index: Data.Integer(),
      root_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Apply: Data.Object({
      hub_reference_index: Data.Integer(),
      operation: EventHistoryOperationSchema,
    }),
  }),
]);
export type EventHistoryObserve = Data.Static<typeof EventHistoryObserveSchema>;
export const EventHistoryObserve = asDataType<EventHistoryObserve>(
  EventHistoryObserveSchema,
);

/** The delegated witness and list observer must bind exactly these indexes. */
export const eventHistoryRetirementOperation = (
  witness: EventHistoryRetirementWitness,
): EventHistoryOperation => ({
  RetireOrder: {
    predecessor_output_index: witness.predecessor_output_index,
    funds_output_index: witness.funds_output_index,
    structural_refund_output_index: witness.structural_refund_output_index,
  },
});

/** Provisional recipe: callers must supply measured bounds, never implicit defaults. */
export type EventHistoryRecipe = {
  readonly hubPolicyId: string;
  readonly kind: EventHistoryKind;
  readonly initializationNonce: OutputReference;
  readonly protectionDurationMs: bigint;
  readonly inlineLimitBytes: bigint;
  readonly maxPayloadBytes: bigint;
  readonly maxPayloadNodes: bigint;
};

export const applyEventHistoryValidators = (
  blueprint: FaultProofBlueprint,
  network: Network,
  recipe: EventHistoryRecipe,
) => {
  if (
    recipe.protectionDurationMs <= 0n ||
    recipe.inlineLimitBytes <= 0n ||
    recipe.maxPayloadBytes < recipe.inlineLimitBytes ||
    recipe.maxPayloadNodes <= 0n
  ) {
    throw new Error(
      "Event history requires positive measured protection, byte and Data-node bounds",
    );
  }
  const retention = applyEventHistoryRetentionValidator(
    blueprint,
    network,
    recipe.hubPolicyId,
    recipe.kind,
  );
  const retentionAddress = Effect.runSync(
    addressDataFromBech32(retention.address),
  );
  const retirementValidator: SpendingValidator = {
    type: "PlutusV3",
    script: applyBlueprintParams(
      blueprint,
      EVENT_HISTORY_CONTRACT_TITLES.retirement,
      [
        recipe.hubPolicyId,
        Data.from(Data.to(recipe.kind, EventHistoryKind)),
        Data.from(Data.to(retentionAddress, AddressData)),
        recipe.protectionDurationMs,
        recipe.inlineLimitBytes,
        recipe.maxPayloadBytes,
        recipe.maxPayloadNodes,
      ],
    ),
  };
  const validator: SpendingValidator = {
    type: "PlutusV3",
    script: applyBlueprintParams(
      blueprint,
      EVENT_HISTORY_CONTRACT_TITLES.spend,
      [
        recipe.hubPolicyId,
        Data.from(Data.to(recipe.kind, EventHistoryKind)),
        Data.from(Data.to(retentionAddress, AddressData)),
        validatorToScriptHash(retirementValidator),
        Data.from(Data.to(recipe.initializationNonce, OutputReference)),
        recipe.protectionDurationMs,
        recipe.inlineLimitBytes,
        recipe.maxPayloadBytes,
        recipe.maxPayloadNodes,
      ],
    ),
  };
  return {
    validator,
    policyId: mintingPolicyToId(validator),
    address: validatorToAddress(network, validator),
    rewardAddress: validatorToRewardAddress(network, validator),
    retention,
    retirement: {
      validator: retirementValidator,
      address: validatorToAddress(network, retirementValidator),
      rewardAddress: validatorToRewardAddress(network, retirementValidator),
    },
  };
};
