/**
 * Snapshots of the ledger an event-ledger walk gives, as a ledger store
 * `Ledger` or as a checked view, comparable with each other: the root, and
 * for each probe its output, its resolution proof and its SDK proof.
 */
import {
  type CheckedLedger,
  checkedLedgerPreState,
} from "../../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  ledgerProof,
} from "../../src/ledger-store/index.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
} from "../../src/transition-trace/phas.js";
import { hex } from "./event-ledger-seeded-blocks.js";

/** `checked` admitted to a memory store: the same ledger as a `Ledger`. */
export const admitted = async (checked: CheckedLedger) =>
  await createLedgerStore(createMemoryLedgerStorage()).admit(
    checked.entries,
    checked.utxosRoot,
  );

/** The SDK proof of `key` against the view's trie, as families build it. */
const viewProof = async (view: CheckedLedger, key: Buffer) => {
  const leaf = view.trie.entries.find((entry) => entry.key.equals(key));
  return leaf === undefined
    ? await keyValuePhasNonMembershipProof(view.trie, key)
    : await keyValuePhasProof(view.trie, key, leaf.value);
};

export const viewSnapshot = async (
  view: CheckedLedger,
  probes: readonly Buffer[],
) => {
  const proofs = await checkedLedgerPreState(view).proveResolution(probes);
  return {
    root: view.utxosRoot,
    probes: await Promise.all(
      probes.map(async (key, index) => {
        const output = view.outputs.get(hex(key));
        return [
          output === undefined ? undefined : hex(output),
          hex(proofs[index]!),
          await viewProof(view, key),
        ];
      }),
    ),
  };
};

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

/** The ledger a request gives, snapshotted, or its refusal. */
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
        message: error.message,
        reason,
        step,
      },
    };
  }
  return { ledger: await snapshot(ledger) };
};
