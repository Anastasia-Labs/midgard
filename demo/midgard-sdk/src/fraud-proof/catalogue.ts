import {
  Assets,
  type BuildTxWithRedeemer,
  Data,
  fromText,
  LucidEvolution,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type {
  AuthenticatedValidator,
  FraudProofs,
  SpendingValidator,
} from "@/common.js";
import {
  decodeNestedLinkedListDatum,
  encodeNestedLinkedListDatum,
  nestedLinkedListInnerRootDatum,
  nestedLinkedListNodeDatum,
  nestedLinkedListRootDatum,
  NestedLinkedListDatum,
  NestedLinkedListDatumSchema,
} from "@/nested-linked-list.js";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";

export const FRAUD_PROOF_CATALOGUE_ROOT_ASSET_NAME = fromText(
  "MIDGARD_FRAUD_PROOF_CATALOGUE",
);

export const FRAUD_PROOF_CATALOGUE_NODE_ASSET_NAME_PREFIX = fromText("MFPC");

export const FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT = 4;

export const FRAUD_PROOF_CATALOGUE_STEP_BYTE_COUNT = 4;

export const FRAUD_PROOF_CATALOGUE_FIRST_STEP_ID = 0;

export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = [
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "invalidRange",
] as const satisfies readonly (keyof FraudProofs)[];

export type FraudProofCatalogueCategoryName =
  (typeof FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number];

export type FraudProofCatalogueStepDeploymentInfo = {
  readonly stepId: number;
  readonly stepKey: string;
  readonly scriptHash: string;
};

export type FraudProofCatalogueCategoryDeploymentInfo = {
  readonly fraudProofId: number;
  readonly fraudProofKey: string;
  readonly stepCount: number;
  readonly steps: readonly FraudProofCatalogueStepDeploymentInfo[];
};

export type FraudProofCatalogueDeploymentInfo = {
  readonly categories: Readonly<
    Record<
      FraudProofCatalogueCategoryName,
      FraudProofCatalogueCategoryDeploymentInfo
    >
  >;
};

export const FraudProofCatalogueIsLockedSchema = Data.Boolean();
export type FraudProofCatalogueIsLocked = Data.Static<
  typeof FraudProofCatalogueIsLockedSchema
>;
export const FraudProofCatalogueIsLocked =
  FraudProofCatalogueIsLockedSchema as unknown as FraudProofCatalogueIsLocked;

export const FraudProofCatalogueMetadataSchema = Data.Object({
  step_count: Data.Integer(),
});
export type FraudProofCatalogueMetadata = Data.Static<
  typeof FraudProofCatalogueMetadataSchema
>;
export const FraudProofCatalogueMetadata =
  FraudProofCatalogueMetadataSchema as unknown as FraudProofCatalogueMetadata;

export const FraudProofCatalogueDatumSchema = NestedLinkedListDatumSchema;
export type FraudProofCatalogueDatum = NestedLinkedListDatum;
export const FraudProofCatalogueDatum =
  NestedLinkedListDatum as unknown as FraudProofCatalogueDatum;

export const FraudProofCatalogueMintRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    AddFraudProof: Data.Object({
      fraud_proof_id: Data.Integer(),
      step_output_count: Data.Integer(),
      m_root_ref_input_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    AddFraudProofStep: Data.Object({
      fraud_proof_id: Data.Integer(),
      step_id: Data.Integer(),
      continued_anchor_output_index: Data.Integer(),
      step_output_index: Data.Integer(),
    }),
  }),
]);
export type FraudProofCatalogueMintRedeemer = Data.Static<
  typeof FraudProofCatalogueMintRedeemerSchema
>;
export const FraudProofCatalogueMintRedeemer =
  FraudProofCatalogueMintRedeemerSchema as unknown as FraudProofCatalogueMintRedeemer;

export const FraudProofCatalogueSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    LockCatalogue: Data.Object({
      root_input_index: Data.Integer(),
      continued_root_output_index: Data.Integer(),
    }),
  }),
]);
export type FraudProofCatalogueSpendRedeemer = Data.Static<
  typeof FraudProofCatalogueSpendRedeemerSchema
>;
export const FraudProofCatalogueSpendRedeemer =
  FraudProofCatalogueSpendRedeemerSchema as unknown as FraudProofCatalogueSpendRedeemer;

const FRAUD_PROOF_CATALOGUE_LIST_STATE_TRANSITION_REDEEMER = Data.to(
  "ListStateTransition",
  FraudProofCatalogueSpendRedeemer,
);

type IntegerLike = number | bigint;

export type FraudProofCatalogueReferenceScripts = {
  readonly minting?: UTxO;
  readonly spending?: UTxO;
};

export type FraudProofCatalogueStepOutput = {
  readonly stepId: IntegerLike;
  readonly validator: SpendingValidator;
  readonly lovelace: bigint;
};

export type FraudProofCatalogueInitParams = {
  readonly validator: AuthenticatedValidator;
  readonly genesisAdmin: string;
  readonly rootLovelace: bigint;
  readonly referenceScripts?: FraudProofCatalogueReferenceScripts;
};

export type FraudProofCatalogueAddFraudProofParams = {
  readonly validator: AuthenticatedValidator;
  readonly genesisAdmin: string;
  readonly anchorUTxO: UTxO;
  readonly rootRefUTxO?: UTxO;
  readonly fraudProofId: IntegerLike;
  readonly finalStepCount: IntegerLike;
  readonly innerRootLovelace: bigint;
  readonly steps: readonly [
    FraudProofCatalogueStepOutput,
    ...FraudProofCatalogueStepOutput[],
  ];
  readonly referenceScripts?: FraudProofCatalogueReferenceScripts;
};

export type FraudProofCatalogueAddFraudProofStepParams = {
  readonly validator: AuthenticatedValidator;
  readonly genesisAdmin: string;
  readonly anchorUTxO: UTxO;
  readonly fraudProofId: IntegerLike;
  readonly stepId: IntegerLike;
  readonly stepValidator: SpendingValidator;
  readonly stepLovelace: bigint;
  readonly referenceScripts?: FraudProofCatalogueReferenceScripts;
};

export type FraudProofCatalogueLockParams = {
  readonly validator: AuthenticatedValidator;
  readonly genesisAdmin: string;
  readonly rootUTxO: UTxO;
  readonly referenceScripts?: FraudProofCatalogueReferenceScripts;
};

const integerToBigInt = (value: IntegerLike, label: string): bigint => {
  if (typeof value === "bigint") {
    return value;
  }
  if (!Number.isSafeInteger(value)) {
    throw new Error(`${label} must be a safe integer`);
  }
  return BigInt(value);
};

const uintToFixedBigEndianHex = (
  value: IntegerLike,
  byteCount: number,
  label: string,
): string => {
  const int = integerToBigInt(value, label);
  if (int < 0n) {
    throw new Error(`${label} must be non-negative`);
  }

  const max = 1n << BigInt(byteCount * 8);
  if (int >= max) {
    throw new Error(`${label} does not fit in ${byteCount.toString()} bytes`);
  }

  return int.toString(16).padStart(byteCount * 2, "0");
};

export const fraudProofKeyFromId = (fraudProofId: IntegerLike): string =>
  uintToFixedBigEndianHex(
    fraudProofId,
    FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
    "fraud proof id",
  );

export const fraudProofStepKey = (stepId: IntegerLike): string =>
  uintToFixedBigEndianHex(
    stepId,
    FRAUD_PROOF_CATALOGUE_STEP_BYTE_COUNT,
    "fraud proof step id",
  );

export const fraudProofCatalogueNodeKey = (
  fraudProofKey: string,
  stepId: IntegerLike,
): string => `${fraudProofKey}${fraudProofStepKey(stepId)}`;

export const fraudProofCatalogueNodeAssetName = (nodeKey: string): string =>
  `${FRAUD_PROOF_CATALOGUE_NODE_ASSET_NAME_PREFIX}${nodeKey}`;

export const fraudProofCatalogueRootUnit = (
  validator: AuthenticatedValidator,
): string => toUnit(validator.policyId, FRAUD_PROOF_CATALOGUE_ROOT_ASSET_NAME);

export const fraudProofCatalogueNodeUnit = (
  validator: AuthenticatedValidator,
  nodeKey: string,
): string =>
  toUnit(validator.policyId, fraudProofCatalogueNodeAssetName(nodeKey));

export const fraudProofCatalogueMetadata = (
  stepCount: IntegerLike,
): FraudProofCatalogueMetadata => ({
  step_count: integerToBigInt(stepCount, "fraud proof step count"),
});

export const fraudProofCatalogueRootDatum = (
  isLocked: boolean,
  link: string | null,
): FraudProofCatalogueDatum =>
  nestedLinkedListRootDatum(
    Data.castTo(isLocked, FraudProofCatalogueIsLocked) as Data,
    link,
  );

export const fraudProofCatalogueInnerRootDatum = (
  stepCount: IntegerLike,
  childLink: string | null,
  link: string | null,
): FraudProofCatalogueDatum =>
  nestedLinkedListInnerRootDatum(
    Data.castTo(
      fraudProofCatalogueMetadata(stepCount),
      FraudProofCatalogueMetadata,
    ) as Data,
    childLink,
    link,
  );

export const fraudProofCatalogueStepDatum = (
  link: string | null,
): FraudProofCatalogueDatum => nestedLinkedListNodeDatum("", link);

export const encodeFraudProofCatalogueDatum = (
  datum: FraudProofCatalogueDatum,
): string => encodeNestedLinkedListDatum(datum);

export const decodeFraudProofCatalogueDatum = (
  datum: string,
): FraudProofCatalogueDatum => decodeNestedLinkedListDatum(datum);

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}.${utxo.outputIndex.toString()}`;

const requireFraudProofCatalogueDatum = (
  utxo: UTxO,
  label: string,
): FraudProofCatalogueDatum => {
  if (!utxo.datum) {
    throw new Error(`${label} ${outRefLabel(utxo)} is missing an inline datum`);
  }

  return decodeFraudProofCatalogueDatum(utxo.datum);
};

const requireSingleNonAdaUnit = (assets: Assets, label: string): string => {
  const units = Object.keys(assets).filter((unit) => unit !== "lovelace");
  if (units.length !== 1) {
    throw new Error(
      `${label} must carry exactly one non-lovelace asset, got ${units.length.toString()}`,
    );
  }
  return units[0]!;
};

const referenceScriptInputs = (
  referenceScripts: FraudProofCatalogueReferenceScripts | undefined,
  needs: { readonly minting?: boolean; readonly spending?: boolean },
): UTxO[] => [
  ...(needs.minting && referenceScripts?.minting
    ? [referenceScripts.minting]
    : []),
  ...(needs.spending && referenceScripts?.spending
    ? [referenceScripts.spending]
    : []),
];

const attachMissingScripts = (
  tx: TxBuilder,
  validator: AuthenticatedValidator,
  referenceScripts: FraudProofCatalogueReferenceScripts | undefined,
  needs: { readonly minting?: boolean; readonly spending?: boolean },
): TxBuilder => {
  let nextTx = tx;
  if (needs.minting && !referenceScripts?.minting) {
    nextTx = nextTx.attach.Script(validator.mintingScript);
  }
  if (needs.spending && !referenceScripts?.spending) {
    nextTx = nextTx.attach.Script(validator.spendingScript);
  }
  return nextTx;
};

const applyCatalogueScripts = (
  tx: TxBuilder,
  validator: AuthenticatedValidator,
  referenceScripts: FraudProofCatalogueReferenceScripts | undefined,
  needs: { readonly minting?: boolean; readonly spending?: boolean },
  extraReferenceInputs: readonly UTxO[] = [],
): TxBuilder => {
  const readInputs = [
    ...extraReferenceInputs,
    ...referenceScriptInputs(referenceScripts, needs),
  ];

  const txWithReferenceInputs =
    readInputs.length > 0 ? tx.readFrom(readInputs) : tx;

  return attachMissingScripts(
    txWithReferenceInputs,
    validator,
    referenceScripts,
    needs,
  );
};

type FraudProofCatalogueRootDatum = FraudProofCatalogueDatum & {
  readonly data: { readonly Root: { readonly data: Data } };
};

type FraudProofCatalogueInnerRootDatum = FraudProofCatalogueDatum & {
  readonly data: {
    readonly InnerRoot: {
      readonly data: Data;
      readonly child_link: string | null;
    };
  };
};

const isRootDatum = (
  datum: FraudProofCatalogueDatum,
): datum is FraudProofCatalogueRootDatum => "Root" in datum.data;

const isInnerRootDatum = (
  datum: FraudProofCatalogueDatum,
): datum is FraudProofCatalogueInnerRootDatum => "InnerRoot" in datum.data;

/**
 * Init.
 */
export const incompleteFraudProofCatalogueInitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const rootUnit = fraudProofCatalogueRootUnit(params.validator);
    const assets: Assets = {
      lovelace: params.rootLovelace,
      [rootUnit]: 1n,
    };
    const datum = fraudProofCatalogueRootDatum(false, null);

    const tx = lucid
      .newTx()
      .mintAssets({ [rootUnit]: 1n }, ((ctx) =>
        Data.to(
          {
            Init: {
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) => (output.assets[rootUnit] ?? 0n) === 1n,
                "fraud-proof catalogue root",
              ),
            },
          },
          FraudProofCatalogueMintRedeemer,
        )) satisfies BuildTxWithRedeemer)
      .pay.ToContract(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeFraudProofCatalogueDatum(datum),
        },
        assets,
      )
      .addSignerKey(params.genesisAdmin);

    return applyCatalogueScripts(
      tx,
      params.validator,
      params.referenceScripts,
      { minting: true },
    );
  });

/**
 * AddFraudProof.
 */
export const incompleteFraudProofCatalogueAddFraudProofTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueAddFraudProofParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const anchorDatum = requireFraudProofCatalogueDatum(
      params.anchorUTxO,
      "fraud-proof catalogue anchor",
    );
    const anchorIsRoot = isRootDatum(anchorDatum);
    if (anchorIsRoot && params.rootRefUTxO) {
      throw new Error(
        "fraud-proof catalogue AddFraudProof must not provide a root reference input when the anchor is the root",
      );
    }
    if (!anchorIsRoot && !params.rootRefUTxO) {
      throw new Error(
        "fraud-proof catalogue AddFraudProof requires a root reference input when the anchor is not the root",
      );
    }

    const fraudProofId = integerToBigInt(params.fraudProofId, "fraud proof id");
    const fraudProofKey = fraudProofKeyFromId(fraudProofId);
    const innerRootUnit = fraudProofCatalogueNodeUnit(
      params.validator,
      fraudProofKey,
    );
    const stepNodes = params.steps.map((step) => {
      const nodeKey = fraudProofCatalogueNodeKey(fraudProofKey, step.stepId);
      return {
        ...step,
        nodeKey,
        unit: fraudProofCatalogueNodeUnit(params.validator, nodeKey),
      };
    });
    const firstStep = stepNodes[0]!;

    const spendRedeemer = FRAUD_PROOF_CATALOGUE_LIST_STATE_TRANSITION_REDEEMER;
    const mintedAssets = stepNodes.reduce<Assets>(
      (assets, step) => ({
        ...assets,
        [step.unit]: 1n,
      }),
      { [innerRootUnit]: 1n },
    );
    const continuedAnchorDatum: FraudProofCatalogueDatum = {
      ...anchorDatum,
      link: fraudProofKey,
    };
    const innerRootDatum = fraudProofCatalogueInnerRootDatum(
      params.finalStepCount,
      firstStep.nodeKey,
      anchorDatum.link,
    );

    let tx = lucid
      .newTx()
      .collectFrom([params.anchorUTxO], spendRedeemer)
      .mintAssets(mintedAssets, ((ctx) =>
        Data.to(
          {
            AddFraudProof: {
              fraud_proof_id: fraudProofId,
              step_output_count: BigInt(stepNodes.length),
              m_root_ref_input_index: params.rootRefUTxO
                ? requireReferenceInputIndex(
                    ctx,
                    params.rootRefUTxO,
                    "fraud-proof catalogue root",
                  )
                : null,
            },
          },
          FraudProofCatalogueMintRedeemer,
        )) satisfies BuildTxWithRedeemer)
      .pay.ToContract(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeFraudProofCatalogueDatum(continuedAnchorDatum),
        },
        params.anchorUTxO.assets,
      )
      .pay.ToContract(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeFraudProofCatalogueDatum(innerRootDatum),
        },
        {
          lovelace: params.innerRootLovelace,
          [innerRootUnit]: 1n,
        },
      );

    for (let index = 0; index < stepNodes.length; index += 1) {
      const step = stepNodes[index]!;
      const nextStep = stepNodes[index + 1];
      const stepDatum = fraudProofCatalogueStepDatum(
        nextStep ? nextStep.nodeKey : null,
      );
      tx = tx.pay.ToContract(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeFraudProofCatalogueDatum(stepDatum),
        },
        {
          lovelace: step.lovelace,
          [step.unit]: 1n,
        },
        step.validator.spendingScript,
      );
    }

    tx = tx.addSignerKey(params.genesisAdmin);

    return applyCatalogueScripts(
      tx,
      params.validator,
      params.referenceScripts,
      { minting: true, spending: true },
      params.rootRefUTxO ? [params.rootRefUTxO] : [],
    );
  });

/**
 * AddFraudProofStep.
 */
export const incompleteFraudProofCatalogueAddFraudProofStepTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueAddFraudProofStepParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const anchorDatum = requireFraudProofCatalogueDatum(
      params.anchorUTxO,
      "fraud-proof catalogue inner root",
    );
    if (!isInnerRootDatum(anchorDatum)) {
      throw new Error(
        "fraud-proof catalogue AddFraudProofStep anchor must be an inner root",
      );
    }

    const fraudProofId = integerToBigInt(params.fraudProofId, "fraud proof id");
    const stepId = integerToBigInt(params.stepId, "fraud proof step id");
    const fraudProofKey = fraudProofKeyFromId(fraudProofId);
    const stepKey = fraudProofCatalogueNodeKey(fraudProofKey, stepId);
    const stepUnit = fraudProofCatalogueNodeUnit(params.validator, stepKey);
    const anchorUnit = requireSingleNonAdaUnit(
      params.anchorUTxO.assets,
      "fraud-proof catalogue inner root",
    );
    const continuedAnchorDatum: FraudProofCatalogueDatum = {
      ...anchorDatum,
      data: {
        InnerRoot: {
          ...anchorDatum.data.InnerRoot,
          child_link: stepKey,
        },
      },
    };
    const stepDatum = fraudProofCatalogueStepDatum(
      anchorDatum.data.InnerRoot.child_link,
    );
    const spendRedeemer = FRAUD_PROOF_CATALOGUE_LIST_STATE_TRANSITION_REDEEMER;

    const tx = lucid
      .newTx()
      .collectFrom([params.anchorUTxO], spendRedeemer)
      .mintAssets({ [stepUnit]: 1n }, ((ctx) =>
        Data.to(
          {
            AddFraudProofStep: {
              fraud_proof_id: fraudProofId,
              step_id: stepId,
              continued_anchor_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) => (output.assets[anchorUnit] ?? 0n) === 1n,
                "fraud-proof catalogue continued inner root",
              ),
              step_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) => (output.assets[stepUnit] ?? 0n) === 1n,
                "fraud-proof catalogue step",
              ),
            },
          },
          FraudProofCatalogueMintRedeemer,
        )) satisfies BuildTxWithRedeemer)
      .pay.ToContract(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeFraudProofCatalogueDatum(continuedAnchorDatum),
        },
        params.anchorUTxO.assets,
      )
      .pay.ToContract(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeFraudProofCatalogueDatum(stepDatum),
        },
        {
          lovelace: params.stepLovelace,
          [stepUnit]: 1n,
        },
        params.stepValidator.spendingScript,
      )
      .addSignerKey(params.genesisAdmin);

    return applyCatalogueScripts(
      tx,
      params.validator,
      params.referenceScripts,
      { minting: true, spending: true },
    );
  });

/**
 * LockCatalogue.
 */
export const incompleteFraudProofCatalogueLockTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueLockParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const rootDatum = requireFraudProofCatalogueDatum(
      params.rootUTxO,
      "fraud-proof catalogue root",
    );
    if (!isRootDatum(rootDatum)) {
      throw new Error("fraud-proof catalogue lock input must be the root");
    }

    const rootUnit = fraudProofCatalogueRootUnit(params.validator);
    const continuedRootDatum = fraudProofCatalogueRootDatum(
      true,
      rootDatum.link,
    );

    const tx = lucid
      .newTx()
      .collectFrom([params.rootUTxO], ((ctx) => {
        requireOwnSpendPurpose(
          ctx,
          params.rootUTxO,
          "fraud-proof catalogue lock",
        );

        return Data.to(
          {
            LockCatalogue: {
              root_input_index: requireInputIndex(
                ctx,
                params.rootUTxO,
                "fraud-proof catalogue root",
              ),
              continued_root_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) => (output.assets[rootUnit] ?? 0n) === 1n,
                "fraud-proof catalogue continued root",
              ),
            },
          },
          FraudProofCatalogueSpendRedeemer,
        );
      }) satisfies BuildTxWithRedeemer)
      .pay.ToContract(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeFraudProofCatalogueDatum(continuedRootDatum),
        },
        params.rootUTxO.assets,
      )
      .addSignerKey(params.genesisAdmin);

    return applyCatalogueScripts(
      tx,
      params.validator,
      params.referenceScripts,
      { spending: true },
    );
  });
