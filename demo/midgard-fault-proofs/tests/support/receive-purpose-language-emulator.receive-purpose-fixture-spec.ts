export const FAMILY = "receive-purpose-language";

/**
 * A PlutusV3 program whose receive execution the machine refuses with
 * `E_PLUTUS_SCRIPT_INVALID` ("ReceivingScript requires MidgardV1 context"),
 * and the CEK program-material sidecar that carries it. Shared with the
 * retained-DA unit test so both suites accuse the same committed script.
 */
export const PLUTUS_V3_RECEIVE_SCRIPT = Buffer.from(
  "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
  "hex",
);

export const PLUTUS_V3_RECEIVE_SIDECAR = Buffer.from(
  "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
  "hex",
);

export const RECEIVE_PURPOSE_REJECT_CODE = "E_PLUTUS_SCRIPT_INVALID" as const;

export type ReceivePurposeFixtureSpec = Readonly<{
  /** Whether the accused event is a committed L2 transaction or a forced one. */
  direction: "accepted" | "forced";
  /** The language of the accused receive purpose. */
  language: "plutusV3" | "native";
  /** The verdict the operator committed for the event. */
  claimedVerdict: "accepted" | "rejected";
  /**
   * Total script purposes: one receive purpose (the machine keys receive
   * purposes by receiving script, so one protected output is one purpose)
   * plus `purposeCount - 1` native spend purposes, each a distinct spent
   * out-ref protected by the trivial `all []` script, which is what widens
   * the purpose and execution frontiers. Spend purposes share one witness
   * because the machine's script-discovery bitmap caps distinct script
   * witnesses at 64 per transaction.
   */
  purposeCount: number;
  /**
   * Adds a passing native receive purpose whose script hash sorts before the
   * accused one, so the accused receive is execution 1, not execution 0.
   * Only with a lone PlutusV3 receive.
   */
  leadingNativeReceive?: boolean;
  /**
   * The execution index the operator's forced verdict names; defaults to the
   * accused receive's own.
   */
  committedExecutionIndex?: number;
  /** Extra committed L2 transactions widening the validation-traces trie. */
  decoyTransactionCount?: number;
  /** Byte seeding the spent out-ref so fixtures in one harness stay distinct. */
  inputByte: number;
  operatorVkey: string;
  startTime: bigint;
}>;

export const receivePurposeReason = (executionIndex: number) =>
  ({
    ReceivePurposePlutusV3Forbidden: {
      execution_index: BigInt(executionIndex),
    },
  }) as const;
