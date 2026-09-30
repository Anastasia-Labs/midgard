import {
  type Assets,
  Data,
  LucidEvolution,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  Bech32DeserializationError,
  HashingError,
  LucidError,
  makeReturn,
  MidgardValidators,
  outputReferenceFromUTxO,
} from "../common.js";
import { HubOracleError } from "../hub-oracle.js";
import {
  buildCompletedUserEventMintTxProgram,
  encodeUserEventWitnessMintOrBurnRedeemer,
  outputReferenceToPlutusDataCbor,
  prepareUserEventMintContext,
  userEventAuthenticateMintRedeemer,
  UserEventBuildError,
} from "./internals.js";
import {
  buildUnsignedCekProgramMaterialProgram,
  buildUnsignedCekSinglePublicationProgram,
  DEFAULT_TX_ORDER_LOVELACE,
  type TxOrderBuildMetadata,
} from "./tx-order.build-unsigned-cek-single-publication-program.js";
import {
  deriveTxOrderMaterial,
  planTxOrderMaterialCarriage,
  requireTxOrderCreatorKeyHashProgram,
} from "./tx-order.derive-tx-order-material.js";
import {
  type SubmitTxOrderConfig,
  TxOrderDatum,
  TxOrderMintRedeemer,
} from "./tx-order.submit-tx-order-config.js";
import {
  carriageIsResolvable,
  type PublishCekProgramMaterialConfig,
  type PublishCekSinglePublicationConfig,
  txOrderMaterialCarriageVector,
} from "./tx-order.tx-order-material-carriage-vector.js";

export const buildUnsignedTxOrderTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: TxOrderBuildMetadata;
  },
  | HubOracleError
  | LucidError
  | Bech32DeserializationError
  | HashingError
  | UserEventBuildError
> =>
  Effect.gen(function* () {
    const submittedTxCbor = yield* Effect.try({
      try: () => {
        if (
          config.submittedTxCbor.length === 0 ||
          config.submittedTxCbor.length % 2 !== 0 ||
          !/^[0-9a-f]+$/iu.test(config.submittedTxCbor)
        ) {
          throw new Error(
            "submittedTxCbor must be non-empty, even-length hexadecimal",
          );
        }
        return Buffer.from(config.submittedTxCbor, "hex");
      },
      catch: (cause) =>
        new UserEventBuildError({
          message:
            "V1 tx order requires exact bounded canonical native V1 bytes",
          cause,
        }),
    });
    const context = yield* prepareUserEventMintContext({
      lucid,
      contracts,
      label: "tx order",
      eventPolicyId: contracts.txOrder.policyId,
      hubOraclePolicyField: "tx_order",
      hubOracleAddressField: "tx_order_addr",
      nonceInput: config.nonceInput,
    });
    const {
      eventUnit: txOrderUnit,
      hubOracleRefInput,
      inclusionTime,
      network,
      nonceInput,
      validTo,
      witnessScript,
      witnessScriptHash,
    } = context;
    const txOrderId = outputReferenceFromUTxO(nonceInput);
    const authNonceCbor = outputReferenceToPlutusDataCbor(nonceInput);
    const creatorPaymentKeyHash =
      yield* requireTxOrderCreatorKeyHashProgram(lucid);
    const carriageReferenceInputs = config.carriageReferenceInputs ?? [];
    const order = yield* Effect.try({
      try: () => {
        const material = deriveTxOrderMaterial({
          submittedTxCbor,
          // §8.5/§8.7 under #594's ruling: reclaim is an ordinary key spend at
          // any time after the mint, so the min-Ada authority a tier-3
          // certificate records has to be a key the creator can sign with. The
          // event witness script hash stood here while no plan was publishable
          // (#589); it never was the right authority.
          owner: creatorPaymentKeyHash,
        });
        const plan = planTxOrderMaterialCarriage({
          material,
          owner: creatorPaymentKeyHash,
          inlineReserveBytes: config.inlineCarriageReserveBytes,
        });
        // A tier-3 field needs the §8.6 policy id to name its manifest token, and
        // saying so here names the configuration that is missing instead of
        // failing later on a token nobody could have matched.
        if (
          config.fieldPreimageCertificatePolicyId === undefined &&
          plan.referenced.some((field) => field.plan.tier === "Certified")
        ) {
          throw new Error(
            `forced order carries a field larger than §8.3's K (${plan.referenced
              .filter((field) => field.plan.tier === "Certified")
              .map((field) => field.fieldName)
              .join(", ")}), which is tier-3 carriage, but ` +
              "`fieldPreimageCertificatePolicyId` was not configured — the §8.6 " +
              "certificate policy is what the door checks a manifest against",
          );
        }
        // Say which publications are missing here rather than let the door abort
        // at submission with no name attached. Every referenced field's carriage
        // has to exist *before* this transaction, because reference inputs are
        // resolved against the UTxO set as it stands before it.
        const unpublished = plan.referenced.filter(
          (field) =>
            !carriageIsResolvable(
              field,
              carriageReferenceInputs,
              config.fieldPreimageCertificatePolicyId,
            ),
        );
        if (unpublished.length > 0) {
          throw new Error(
            `forced order references predeployed §8 carriage for ${unpublished
              .map((field) => `${field.fieldName} (tier ${field.plan.tier})`)
              .join(
                ", ",
              )}, which is not among the reference inputs supplied in ` +
              "`carriageReferenceInputs`. Publish each field with " +
              "`buildUnsignedFieldPreimagePublicationV1Program` (and certify " +
              "tier-3 fields) before building the order.",
          );
        }
        return { material, plan };
      },
      catch: (cause) =>
        new UserEventBuildError({
          message: "Failed to derive V1 tx-order material carriage",
          cause,
        }),
    });
    const material = order.material;
    const txOrderDatum: TxOrderDatum = {
      event: {
        id: txOrderId,
        tx: {
          tx_id: material.transactionId,
          transaction_commitment: material.transactionCommitment,
          submitted_source: material.submitted_source,
        },
      },
      inclusion_time: BigInt(inclusionTime),
      witness: witnessScriptHash,
      refund_address: config.refundAddress,
      refund_datum: config.refundDatum ?? "NoDatum",
    };
    const txOrderDatumCBOR = Data.to(txOrderDatum, TxOrderDatum);
    const outputAssets: Assets = {
      lovelace: config.lovelace ?? DEFAULT_TX_ORDER_LOVELACE,
      [txOrderUnit]: 1n,
    };
    const referenceInputs = [
      hubOracleRefInput,
      ...(config.referenceScripts === undefined
        ? []
        : [config.referenceScripts.txOrderMinting]),
      // The order's §8 carriage. Read, never spent — §8.7's content addressing is
      // what makes that safe: a republished chunk with the same bytes is the same
      // carriage, so nothing here is identified by `OutputReference`.
      ...carriageReferenceInputs,
    ];
    const witnessRegistrationRedeemer =
      encodeUserEventWitnessMintOrBurnRedeemer(contracts.txOrder.policyId);
    const tx = yield* buildCompletedUserEventMintTxProgram({
      lucid,
      network,
      nonceInput,
      eventUnit: txOrderUnit,
      eventAddress: contracts.txOrder.spendingScriptAddress,
      eventDatumCbor: txOrderDatumCBOR,
      outputAssets,
      validTo,
      mintingPolicy: contracts.txOrder.mintingScript,
      attachMintingPolicy: config.referenceScripts === undefined,
      referenceInputs,
      hubOracleRefInput,
      witnessScript,
      witnessRegistrationRedeemer,
      label: "tx order",
      // #594: the tx-order policy's redeemer is `user_events.MintRedeemer` inside
      // its own `MintRedeemer`, beside the §8 carriage vector. The indices in that
      // vector are positional in the final transaction's reference-input set, so
      // they are resolved from `ctx` here and not from the order this builder
      // happened to list them in.
      encodeMintRedeemer: ({ layout, ctx }) =>
        Data.to(
          {
            // Delegated, not re-spelled: the four-field mapping is
            // `user_events.MintRedeemer`'s and lives with that type.
            event: userEventAuthenticateMintRedeemer(layout),
            material_carriage: [
              ...txOrderMaterialCarriageVector({
                plan: order.plan,
                certificatePolicyId: config.fieldPreimageCertificatePolicyId,
                referenceInputs: ctx.referenceInputs,
              }),
            ],
          } satisfies TxOrderMintRedeemer,
          TxOrderMintRedeemer,
        ),
    });
    return {
      tx,
      metadata: {
        txOrderAddress: contracts.txOrder.spendingScriptAddress,
        materialCarriage: order.plan,
        txOrderId,
        authNonceCbor,
        txOrderAuthUnit: txOrderUnit,
        nonceInput,
        validTo,
        inclusionTime,
      },
    };
  }).pipe(
    Effect.catchAllDefect((defect) =>
      Effect.fail(
        new LucidError({
          message: "Caught defect from V1 txOrderTxBuilder",
          cause: defect,
        }),
      ),
    ),
  );

export const unsignedTxOrderTxProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderConfig,
) =>
  buildUnsignedTxOrderTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const unsignedTxOrderTx = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  txOrderParams: SubmitTxOrderConfig,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedTxOrderTxProgram(lucid, contracts, txOrderParams),
  ).unsafeRun();

export const unsignedCekProgramMaterial = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekProgramMaterialConfig,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedCekProgramMaterialProgram(lucid, contracts, config),
  ).unsafeRun();

export const unsignedCekSinglePublication = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekSinglePublicationConfig,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedCekSinglePublicationProgram(lucid, contracts, config),
  ).unsafeRun();
