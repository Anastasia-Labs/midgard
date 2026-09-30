import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  MinAdaStep02DatumSchema,
  MinAdaStep02SpendRedeemerSchema,
  MinAdaStep03DatumSchema,
  MinAdaStep05DatumSchema,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { type ResolvedProverSigner } from "../runtime.js";
import {
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContracts,
} from "./contracts.js";

export type State = NonNullable<
  Data.Static<typeof MinAdaStep02DatumSchema>["data"]
>;

export type Step02Datum = Data.Static<typeof MinAdaStep02DatumSchema>;

export const Step02Datum = asDataType<Step02Datum>(MinAdaStep02DatumSchema);

export type Step03Datum = Data.Static<typeof MinAdaStep03DatumSchema>;

export const Step03Datum = asDataType<Step03Datum>(MinAdaStep03DatumSchema);

type Step05Datum = Data.Static<typeof MinAdaStep05DatumSchema>;

const Step05Datum = asDataType<Step05Datum>(MinAdaStep05DatumSchema);

export type Redeemer = Data.Static<typeof MinAdaStep02SpendRedeemerSchema>;

export const Redeemer = asDataType<Redeemer>(MinAdaStep02SpendRedeemerSchema);

export const walletInputsExcludingReferences = ({
  walletUtxos,
  references,
}: {
  readonly walletUtxos: readonly UTxO[];
  readonly references: readonly UTxO[];
}): UTxO[] =>
  walletUtxos.filter(
    (utxo) =>
      !references.some(
        (reference) =>
          reference.txHash === utxo.txHash &&
          reference.outputIndex === utxo.outputIndex,
      ),
  );

export const requireStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer,
    schema: Step02Datum,
    family: FAMILY,
    stepIndex,
  });
  return { stepIndex, threadUtxo, threadToken, state };
};
