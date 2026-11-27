/**
 * Snapshots of the ledger an event-ledger walk gives and of the ledger the
 * from-scratch reference rebuilds, comparable with each other: the root, and
 * for each probe its output, its resolution proof and its SDK proof.
 */
import { type Ledger, ledgerProof } from "../../src/ledger-store/index.js";
import { sdkProofFromCbor } from "../../src/transition-trace/phas.js";
import {
  type ReferenceLedger,
  referenceProofs,
} from "./event-ledger-reference.js";
import { hex } from "./event-ledger-seeded-blocks.js";

export const ledgerSnapshot = async (
  ledger: Ledger,
  probes: readonly Buffer[],
) => {
  const read = await ledger.read(probes);
  return {
    root: ledger.root,
    probes: await Promise.all(
      probes.map(async (key) => {
        const { output, proof } = read.get(hex(key))!;
        return [
          output === undefined ? undefined : hex(output),
          hex(await proof()),
          await ledgerProof(ledger, key),
        ];
      }),
    ),
  };
};

/** The reference's snapshot: proofs from the MPF library's own trie of the
 * reference's entries. */
export const referenceSnapshot = async (
  reference: ReferenceLedger,
  probes: readonly Buffer[],
) => {
  const proofs = await referenceProofs(reference, probes);
  return {
    root: reference.utxosRoot,
    probes: probes.map((key, index) => {
      const output = reference.outputs.get(hex(key));
      return [
        output === undefined ? undefined : hex(output),
        hex(proofs[index]!),
        sdkProofFromCbor(proofs[index]!),
      ];
    }),
  };
};

export type Outcome =
  | Readonly<{ ledger: unknown }>
  | Readonly<{
      refused: Readonly<{
        name: string;
        message: string;
        reason: unknown;
        step: unknown;
      }>;
    }>;

/** The ledger a request gives, snapshotted, or its refusal: its class, its
 * reason and step, and its message without the caller's label. */
export const outcome = async <L>(
  run: () => Promise<L>,
  snapshot: (ledger: L) => Promise<unknown>,
): Promise<Outcome> => {
  let ledger: L;
  try {
    ledger = await run();
  } catch (error) {
    if (!(error instanceof Error)) throw error;
    const { reason, step } = error as Error & {
      reason?: unknown;
      step?: unknown;
    };
    return {
      refused: {
        name: error.constructor.name,
        message: error.message.replace(/^[^:]*: /, ""),
        reason,
        step,
      },
    };
  }
  return { ledger: await snapshot(ledger) };
};
