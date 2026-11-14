/** The PlutusV3 scripts a Plutus-execution block's spends run, and the
 * execution units admission allows them. */
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { plutusV3ScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";

/** `(program 1.1.0 (lam ctx (error)))` in Flat, wrapped in one CBOR byte string. */
export const FAILING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(Buffer.from("0101002601", "hex")),
);
/** `(program 1.1.0 (lam ctx (con unit ())))` in Flat, wrapped in one CBOR byte string. */
export const PASSING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(Buffer.from("0101002499", "hex")),
);

/** The per-transaction execution-unit cap admission enforces (the consensus
 * profile's Cardano `maxTxExUnits`), as `[memory, steps]`. */
export const ADMITTED_TX_EX_UNITS = [
  BigInt(MIDGARD_CONSENSUS_LIMITS.minSupportedTransactionExecutionMemoryUnits),
  BigInt(MIDGARD_CONSENSUS_LIMITS.minSupportedTransactionExecutionCpuUnits),
] as const;

/**
 * A passing PlutusV3 script that runs as long as admission allows:
 * `(program 1.1.0 (lam ctx [(lam _ (con unit ())) (countdown 5148 0)]))`,
 * where `countdown n acc = n == 0 ? acc : countdown (n - 1) acc` through the
 * strict fixpoint. 5148 rounds is the most whose run fits
 * `ADMITTED_TX_EX_UNITS` (16,497,794 memory, 391,317 CEK steps); in Flat,
 * wrapped in one CBOR byte string.
 */
export const LONGEST_PASSING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(
    Buffer.from(
      "010100232498ccc8c8c0088cc0080080048c0088cc0080080048894ccd5cd19b870024800040044cc00ccdc080124004002905c2824001",
      "hex",
    ),
  ),
);

/** A script spend's own script, in place of the scenario's, and, when
 * given, its declared execution units in place of the default units. */
export type ScriptSpend = Readonly<{
  script: MidgardVersionedScript;
  exUnits?: readonly [bigint, bigint];
}>;
