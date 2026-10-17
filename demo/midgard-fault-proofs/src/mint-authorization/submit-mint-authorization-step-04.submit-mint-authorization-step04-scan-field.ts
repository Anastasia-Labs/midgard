import type { MintAuthorizationStep04State } from "@al-ft/midgard-sdk";
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";

import { mintAuthorizationSubmitError } from "./submit-common.js";

export type SubmitMintAuthorizationStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly nextStepAddress: string;
  /**
   * The field and cursor the thread carries after this transaction, or
   * `null` once the scan closes to step-05.
   */
  readonly nextScan: {
    readonly fieldIndex: bigint;
    readonly cursor: bigint;
  } | null;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

/** The input field the thread's state names: field 1, then field 0. */
export const step04ScanField = (
  items: {
    readonly referenceInputsItemCbors: readonly string[];
    readonly spendInputsItemCbors: readonly string[];
  },
  state: MintAuthorizationStep04State,
) => {
  if (state.field_index === BigInt(MIDGARD_FIELD_INDEX.referenceInputs))
    return {
      fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
      itemCbors: items.referenceInputsItemCbors,
      label: "reference-inputs",
    } as const;
  if (state.field_index === BigInt(MIDGARD_FIELD_INDEX.spendInputs))
    return {
      fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
      itemCbors: items.spendInputsItemCbors,
      label: "spend-inputs",
    } as const;
  throw mintAuthorizationSubmitError(
    `step-04 thread names input field ${state.field_index.toString()}, not 0 or 1.`,
  );
};
