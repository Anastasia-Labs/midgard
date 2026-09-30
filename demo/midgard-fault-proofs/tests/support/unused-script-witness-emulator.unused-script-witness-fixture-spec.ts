import { type MidgardNativeScript } from "@al-ft/midgard-core";

export const FAMILY = "unused-script-witness";

/** The rejection code the machine commits for an unused inline script. */
export const UNUSED_SCRIPT_WITNESS_REJECT_CODE =
  "E_INVALID_FIELD_TYPE" as const;

export type UnusedScriptWitnessFixtureSpec = Readonly<{
  /** Whether the accused event is a committed L2 transaction or a forced one. */
  direction: "accepted" | "forced";
  /** The verdict the operator committed for the event. */
  claimedVerdict: "accepted" | "rejected";
  /**
   * Whether the accused (last) inline script is selected by no purpose. When
   * true the machine stops its stage-11 source audit at that coordinate; when
   * false every inline source is used and the machine reaches the stage-12
   * terminal.
   */
  accusedUnused: boolean;
  /**
   * Inline field-6 scripts, each a distinct always-true native script. Every
   * script before the accused one is selected by its own spend purpose, so the
   * alternate-source walk of step 04 carries `sourceCount - 1` openings.
   */
  sourceCount: number;
  /** The accused field-6 coordinate; defaults to the last inline script. */
  accusedIndex?: number;
  /**
   * Further spend purposes under the first script, widening the purpose
   * frontier the reverse match of step 05 walks.
   */
  extraSpendPurposes?: number;
  /** Mint, receive and observer purposes under the second, third and fourth scripts. */
  allPurposeKinds?: boolean;
  /** Extra committed L2 transactions widening the validation-traces trie. */
  decoyTransactionCount?: number;
  /** Byte seeding the spent out-refs so fixtures in one harness stay distinct. */
  inputByte: number;
  operatorVkey: string;
  startTime: bigint;
}>;

export const unusedScriptWitnessReason = (scriptIndex: number) =>
  ({ UnusedScriptWitness: { script_index: BigInt(scriptIndex) } }) as const;

/** Distinct always-true native scripts: `all` over `index` trivial children. */
export const trivialNativeScript = (index: number): MidgardNativeScript => ({
  type: "all",
  scripts: Array.from({ length: index }, () => ({
    type: "all" as const,
    scripts: [],
  })),
});
