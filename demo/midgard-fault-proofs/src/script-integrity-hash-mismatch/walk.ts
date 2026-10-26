/** The all-purposes walk of the `scriptIntegrityHashMismatch` thread, from
 * the family entry to the terminal's input: spends, mint policies, required
 * observers, then protected-script receive outputs, folding the language set
 * of every execution. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardMintPolicyItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  directOutputAddress,
  type DirectScriptMaterial,
} from "../direct-script-derivation/index.js";
import { grammarPages, pages, stepDatum } from "../missing-redeemer/custody.js";
import type { MissingRedeemerSourceEvidence } from "../missing-redeemer/source-walk.js";
import { keyValuePhasMembershipProofs } from "../transition-trace/phas.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL } from "./contracts.js";
import type {
  ScriptIntegrityHashMismatchFieldPlan,
  ScriptIntegrityHashMismatchTransition,
} from "./custody.js";

const LABEL = SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL;

/** Items one walk transaction takes, per purpose kind and for grammar
 * certification. */
export type ScriptIntegrityHashMismatchWalkBatch = Readonly<{
  spend: number;
  mint: number;
  observe: number;
  receive: number;
  grammar: number;
}>;

export const SCRIPT_INTEGRITY_HASH_MISMATCH_WALK_BATCH: ScriptIntegrityHashMismatchWalkBatch =
  Object.freeze({ spend: 8, mint: 8, observe: 8, receive: 8, grammar: 8 });

type Execution = SDK.DirectAllPurposesExecutionWitness;

/** The selected source of `scriptHash` with its source-map proof. A purpose
 * whose script has no source cannot complete the walk. */
const executionOf = async (
  evidence: MissingRedeemerSourceEvidence,
  scriptHash: string,
): Promise<Execution> => {
  const { selected, proof } = await evidence.lookup(scriptHash);
  if (selected === null)
    throw new Error(`${LABEL}: script ${scriptHash} has no source`);
  return { source: selected, proof };
};

/** Each spent item's predecessor descriptor and ledger membership, in raw
 * field order, with the execution of a script-locked input. */
const spendWitnesses = async (
  material: DirectScriptMaterial,
  evidence: MissingRedeemerSourceEvidence,
): Promise<readonly SDK.DirectAllPurposesSpendWitness[]> => {
  const descriptors = material.fields[0]!.map((item) => {
    const entry = material.ledger.root.entries.find((candidate) =>
      candidate.key.equals(item),
    );
    if (entry === undefined)
      throw new Error(`${LABEL}: spent descriptor absent`);
    return entry;
  });
  const proofs = await keyValuePhasMembershipProofs(
    material.ledger.root,
    descriptors,
  );
  return await Promise.all(
    descriptors.map(async (entry, index) => {
      const credential = decodeMidgardAddressBytes(
        decodeMidgardLedgerOutputCommitment(entry.value).address,
      ).paymentCredential;
      return {
        descriptor_cbor: entry.value.toString("hex"),
        proof: proofs[index]!,
        execution:
          credential.kind === "Script"
            ? await executionOf(evidence, credential.hash.toString("hex"))
            : null,
      };
    }),
  );
};

/** The language and probe facts of a whole walk, as the terminal reads them;
 * native executions add no language. */
export type ScriptIntegrityHashMismatchWalk = Readonly<{
  transitions: readonly ScriptIntegrityHashMismatchTransition[];
  final: SDK.DirectAllPurposesState;
}>;

export const scriptIntegrityHashMismatchWalkTransitions = async ({
  material,
  evidence,
  prover,
  plan,
  spendScriptHash,
  batch = SCRIPT_INTEGRITY_HASH_MISMATCH_WALK_BATCH,
}: {
  readonly material: DirectScriptMaterial;
  readonly evidence: MissingRedeemerSourceEvidence;
  readonly prover: string;
  readonly plan: (field: number) => ScriptIntegrityHashMismatchFieldPlan;
  /** The spend walk's own hash, recorded as the resume hash of each take. */
  readonly spendScriptHash: string;
  readonly batch?: ScriptIntegrityHashMismatchWalkBatch;
}): Promise<ScriptIntegrityHashMismatchWalk> => {
  const out: ScriptIntegrityHashMismatchTransition[] = [];
  const walkDatum = (state: SDK.DirectAllPurposesState) =>
    stepDatum(prover, state, SDK.DirectAllPurposesDatum);
  const admit = (
    facts: SDK.DirectAllPurposesFacts,
    executions: readonly (Execution | null)[],
  ) =>
    executions.reduce(
      (folded, execution) =>
        execution === null
          ? folded
          : SDK.admitAllPurposesExecution(folded, {
              scriptHash: execution.source.script_hash,
              languageTag: execution.source.language_tag,
            }),
      facts,
    );
  const counts = (field: number) => plan(field).items.length;

  // Spend field: fixed stride, walking from the entry.
  let state: SDK.DirectAllPurposesState = {
    cursor: plan(0).position("Walking", 0).cursor,
    source_index: evidence.index,
    facts: { language_bitmap: 0n, probe: null, probe_reached: false },
    complete: counts(0) === 0,
  };
  out.push({
    label: "entry",
    role: "familyEntry",
    next: state.complete ? "allPurposesMintGrammar" : "allPurposesSpend",
    datum: walkDatum(state),
    fields: [0],
    redeemer: ({ indices, open }) =>
      Data.to(
        { Continue: [{ ...indices, opening: open(0) }] },
        SDK.ScriptIntegrityHashMismatchEntryRedeemer,
      ),
  });
  const spends = await spendWitnesses(material, evidence);
  for (const [start, budget] of pages(counts(0), batch.spend)) {
    const end = start + budget;
    const taken = {
      ...state,
      cursor: plan(0).position("Walking", end).cursor,
      complete: end === counts(0),
    };
    out.push({
      label: `spend take ${start.toString()}`,
      role: "allPurposesSpend",
      next: "allPurposesSpendResolve",
      datum: stepDatum(
        prover,
        {
          resume_script_hash: spendScriptHash,
          state: taken,
          pending: plan(0)
            .items.slice(start, end)
            .map((item) => item.toString("hex")),
        },
        SDK.DirectAllPurposesPendingDatum,
      ),
      fields: [0],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                opening: open(0),
                checkpoint_bytes: plan(0).position("Walking", start)
                  .checkpointBytes,
                count: BigInt(budget),
              },
            ],
          },
          SDK.DirectAllPurposesSpendTakeRedeemer,
        ),
    });
    const witnesses = spends.slice(start, end);
    state = {
      ...taken,
      facts: admit(
        taken.facts,
        witnesses.map((witness) => witness.execution),
      ),
    };
    out.push({
      label: `spend resolve ${start.toString()}`,
      role: "allPurposesSpendResolve",
      next: state.complete ? "allPurposesMintGrammar" : "allPurposesSpend",
      datum: walkDatum(state),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ ...indices, witnesses: [...witnesses] }] },
          SDK.DirectAllPurposesSpendResolveRedeemer,
        ),
    });
  }

  // Mint field: variable, certified before it is walked.
  state = {
    ...state,
    cursor: plan(5).position("Certifying", 0).cursor,
    complete: false,
  };
  out.push({
    label: "mint open",
    role: "allPurposesMintGrammar",
    next: "allPurposesMintGrammar",
    datum: walkDatum(state),
    fields: [5],
    redeemer: ({ indices, open }) =>
      Data.to(
        { Continue: [{ Open: { ...indices, opening: open(5) } }] },
        SDK.DirectAllPurposesGrammarRedeemer,
      ),
  });
  const certify = (
    field: 5 | 2,
    role: "allPurposesMintGrammar" | "allPurposesReceiveGrammar",
    walking: ScriptIntegrityHashMismatchTransition["next"],
    exhausted: ScriptIntegrityHashMismatchTransition["next"],
  ) => {
    const count = counts(field);
    for (const [start, budget] of grammarPages(count, batch.grammar)) {
      const last = start + budget >= count;
      state = last
        ? {
            ...state,
            cursor: plan(field).position("Walking", 0).cursor,
            complete: count === 0,
          }
        : {
            ...state,
            cursor: plan(field).position("Certifying", start + budget).cursor,
          };
      out.push({
        label: `field ${field.toString()} grammar ${start.toString()}`,
        role,
        next: last ? (count === 0 ? exhausted : walking) : role,
        datum: walkDatum(state),
        fields: [field],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                {
                  Certify: {
                    ...indices,
                    opening: open(field),
                    checkpoint_bytes: plan(field).position("Certifying", start)
                      .checkpointBytes,
                    budget: BigInt(budget),
                  },
                },
              ],
            },
            SDK.DirectAllPurposesGrammarRedeemer,
          ),
      });
    }
  };
  certify(
    5,
    "allPurposesMintGrammar",
    "allPurposesMint",
    "allPurposesObserveOpen",
  );
  const mints = await Promise.all(
    material.fields[5]!.map(
      async (item) =>
        await executionOf(
          evidence,
          Buffer.from(decodeMidgardMintPolicyItem(item).policyId).toString(
            "hex",
          ),
        ),
    ),
  );
  const walkPages = <W>(
    field: 5 | 3 | 2,
    role: "allPurposesMint" | "allPurposesObserve" | "allPurposesReceive",
    size: number,
    witnesses: readonly W[],
    exhausted: ScriptIntegrityHashMismatchTransition["next"],
    redeemer: (args: Record<string, unknown>, page: readonly W[]) => string,
    executions: (page: readonly W[]) => readonly (Execution | null)[],
  ) => {
    const count = counts(field);
    for (const [start, budget] of pages(count, size)) {
      const end = start + budget;
      const page = witnesses.slice(start, end);
      state = {
        ...state,
        cursor: plan(field).position("Walking", end).cursor,
        facts: admit(state.facts, executions(page)),
        complete: end === count,
      };
      out.push({
        label: `field ${field.toString()} walk ${start.toString()}`,
        role,
        next: state.complete ? exhausted : role,
        datum: walkDatum(state),
        fields: [field],
        redeemer: ({ indices, open }) =>
          redeemer(
            {
              ...indices,
              opening: open(field),
              checkpoint_bytes: plan(field).position("Walking", start)
                .checkpointBytes,
            },
            page,
          ),
      });
    }
  };
  walkPages(
    5,
    "allPurposesMint",
    batch.mint,
    mints,
    "allPurposesObserveOpen",
    (args, page) =>
      Data.to(
        { Continue: [{ ...args, witnesses: [...page] }] } as never,
        SDK.DirectAllPurposesExecutionRedeemer,
      ),
    (page) => page,
  );

  // Required observers: fixed stride, walking from their opening.
  state = {
    ...state,
    cursor: plan(3).position("Walking", 0).cursor,
    complete: counts(3) === 0,
  };
  out.push({
    label: "observers open",
    role: "allPurposesObserveOpen",
    next: state.complete ? "allPurposesReceiveGrammar" : "allPurposesObserve",
    datum: walkDatum(state),
    fields: [3],
    redeemer: ({ indices, open }) =>
      Data.to(
        { Continue: [{ ...indices, opening: open(3) }] },
        SDK.DirectAllPurposesOpenRedeemer,
      ),
  });
  const observers = await Promise.all(
    material.fields[3]!.map(
      async (item) => await executionOf(evidence, item.toString("hex")),
    ),
  );
  walkPages(
    3,
    "allPurposesObserve",
    batch.observe,
    observers,
    "allPurposesReceiveGrammar",
    (args, page) =>
      Data.to(
        { Continue: [{ ...args, witnesses: [...page] }] } as never,
        SDK.DirectAllPurposesExecutionRedeemer,
      ),
    (page) => page,
  );

  // Receive outputs: variable, certified before they are walked.
  state = {
    ...state,
    cursor: plan(2).position("Certifying", 0).cursor,
    complete: false,
  };
  out.push({
    label: "receive open",
    role: "allPurposesReceiveGrammar",
    next: "allPurposesReceiveGrammar",
    datum: walkDatum(state),
    fields: [2],
    redeemer: ({ indices, open }) =>
      Data.to(
        { Continue: [{ Open: { ...indices, opening: open(2) } }] },
        SDK.DirectAllPurposesGrammarRedeemer,
      ),
  });
  certify(2, "allPurposesReceiveGrammar", "allPurposesReceive", "terminal");
  const receives = await Promise.all(
    material.fields[2]!.map(async (item) => {
      const address = directOutputAddress(item);
      return address.protected && address.paymentCredential.kind === "Script"
        ? await executionOf(
            evidence,
            address.paymentCredential.hash.toString("hex"),
          )
        : null;
    }),
  );
  walkPages(
    2,
    "allPurposesReceive",
    batch.receive,
    receives,
    "terminal",
    (args, page) =>
      Data.to(
        { Continue: [{ ...args, witnesses: [...page] }] } as never,
        SDK.DirectAllPurposesReceiveRedeemer,
      ),
    (page) => page,
  );
  if (
    !state.complete ||
    state.cursor.field_index !== 2n ||
    out.at(-1)!.next !== "terminal"
  )
    throw new Error(`${LABEL}: the walk does not reach the terminal`);
  return { transitions: out, final: state };
};
