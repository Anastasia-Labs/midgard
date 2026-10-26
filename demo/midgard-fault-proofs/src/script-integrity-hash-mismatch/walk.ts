/** The all-purposes walk of the `scriptIntegrityHashMismatch` thread, from
 * the family entry to the terminal's input: spends, mint policies, required
 * observers, then protected-script receive outputs, folding the language set
 * of every execution. */
import { decodeMidgardMintPolicyItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  directOutputAddress,
  type DirectScriptMaterial,
} from "../direct-script-derivation/index.js";
import { grammarPages, pages, stepDatum } from "../missing-redeemer/custody.js";
import type { MissingRedeemerSourceEvidence } from "../missing-redeemer/source-walk.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL } from "./contracts.js";
import type {
  ScriptIntegrityHashMismatchFieldPlan,
  ScriptIntegrityHashMismatchTransition,
} from "./custody.js";
import {
  type Execution,
  executionOf,
  spendWitnesses,
} from "./walk.executions.js";

const LABEL = SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL;

/** The most items one walk transaction takes, per purpose kind and for
 * grammar certification. A purpose page that exceeds one transaction's budget
 * is taken smaller (`fitScriptIntegrityHashMismatchPage`). */
export type ScriptIntegrityHashMismatchWalkBatch = Readonly<{
  spend: number;
  mint: number;
  observe: number;
  receive: number;
  grammar: number;
}>;

export const SCRIPT_INTEGRITY_HASH_MISMATCH_WALK_BATCH: ScriptIntegrityHashMismatchWalkBatch =
  Object.freeze({ spend: 8, mint: 8, observe: 8, receive: 8, grammar: 8 });

/** The walk roles whose pages open MPF proofs, one or two per item. A spend
 * page is taken here and resolved by `allPurposesSpendResolve`. */
export type ScriptIntegrityHashMismatchPagedRole =
  | "allPurposesSpend"
  | "allPurposesMint"
  | "allPurposesObserve"
  | "allPurposesReceive";

const PAGED = {
  allPurposesSpend: { field: 0, batch: "spend" },
  allPurposesMint: { field: 5, batch: "mint" },
  allPurposesObserve: { field: 3, batch: "observe" },
  allPurposesReceive: { field: 2, batch: "receive" },
} as const;

export const isScriptIntegrityHashMismatchPagedRole = (
  role: string,
): role is ScriptIntegrityHashMismatchPagedRole => Object.hasOwn(PAGED, role);

/** One page of a paged role at the size taken: the take and its resolve for
 * spends, one walk transaction otherwise. */
export type ScriptIntegrityHashMismatchPage = Readonly<{
  size: number;
  transitions: readonly ScriptIntegrityHashMismatchTransition[];
}>;

/** The language and probe facts of a whole walk, as the terminal reads them;
 * native executions add no language. */
export type ScriptIntegrityHashMismatchWalk = Readonly<{
  /** The walk at the configured page sizes. */
  transitions: readonly ScriptIntegrityHashMismatchTransition[];
  final: SDK.DirectAllPurposesState;
  /** The page continuing a thread that holds `datum` at `role`, of `size`
   * items (the configured maximum by default), never more than remain. */
  page: (
    role: ScriptIntegrityHashMismatchPagedRole,
    datum: string,
    size?: number,
  ) => ScriptIntegrityHashMismatchPage;
  /** The transition continuing a thread that holds `datum` at a paged role,
   * at the configured maximum, or the resolve of the take that wrote
   * `datum`. */
  transitionAt: (
    role: ScriptIntegrityHashMismatchPagedRole | "allPurposesSpendResolve",
    datum: string,
  ) => ScriptIntegrityHashMismatchTransition;
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
  const counts = (field: number) => plan(field).items.length;

  const spends = await spendWitnesses(material, evidence);
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
  const observers = await Promise.all(
    material.fields[3]!.map(
      async (item) => await executionOf(evidence, item.toString("hex")),
    ),
  );
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

  // The facts before each item of each purpose field, in walk order (spends,
  // mints, observers, receives); a field's facts start where the previous
  // field's end.
  const factsBefore = new Map<number, readonly SDK.DirectAllPurposesFacts[]>();
  const fold = (
    field: number,
    start: SDK.DirectAllPurposesFacts,
    executions: readonly (Execution | null)[],
  ) => {
    const folded = [start];
    for (const execution of executions) {
      const facts = folded.at(-1)!;
      folded.push(
        execution === null
          ? facts
          : SDK.admitAllPurposesExecution(facts, {
              scriptHash: execution.source.script_hash,
              languageTag: execution.source.language_tag,
            }),
      );
    }
    factsBefore.set(field, folded);
    return folded.at(-1)!;
  };
  fold(
    2,
    fold(
      3,
      fold(
        5,
        fold(
          0,
          { language_bitmap: 0n, probe: null, probe_reached: false },
          spends.map((witness) => witness.execution),
        ),
        mints,
      ),
      observers,
    ),
    receives,
  );

  /** The walk state with the first `index` items of `field` walked. */
  const walking = (
    field: number,
    index: number,
  ): SDK.DirectAllPurposesState => ({
    cursor: plan(field).position("Walking", index).cursor,
    source_index: evidence.index,
    facts: factsBefore.get(field)![index]!,
    complete: index === counts(field),
  });

  const take = (
    start: number,
    size: number,
  ): ScriptIntegrityHashMismatchTransition => {
    const end = start + size;
    return {
      label: `spend take ${start.toString()} (${size.toString()})`,
      role: "allPurposesSpend",
      next: "allPurposesSpendResolve",
      datum: stepDatum(
        prover,
        {
          resume_script_hash: spendScriptHash,
          state: { ...walking(0, end), facts: walking(0, start).facts },
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
                count: BigInt(size),
              },
            ],
          },
          SDK.DirectAllPurposesSpendTakeRedeemer,
        ),
    };
  };
  const resolve = (
    start: number,
    size: number,
  ): ScriptIntegrityHashMismatchTransition => {
    const state = walking(0, start + size);
    const witnesses = spends.slice(start, start + size);
    return {
      label: `spend resolve ${start.toString()} (${size.toString()})`,
      role: "allPurposesSpendResolve",
      next: state.complete ? "allPurposesMintGrammar" : "allPurposesSpend",
      datum: walkDatum(state),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ ...indices, witnesses: [...witnesses] }] },
          SDK.DirectAllPurposesSpendResolveRedeemer,
        ),
    };
  };
  const EXHAUSTED = {
    allPurposesMint: "allPurposesObserveOpen",
    allPurposesObserve: "allPurposesReceiveGrammar",
    allPurposesReceive: "terminal",
  } as const;
  const walkPage = (
    role: Exclude<ScriptIntegrityHashMismatchPagedRole, "allPurposesSpend">,
    start: number,
    size: number,
  ): ScriptIntegrityHashMismatchTransition => {
    const { field } = PAGED[role];
    const state = walking(field, start + size);
    const witnesses =
      role === "allPurposesMint"
        ? mints
        : role === "allPurposesObserve"
          ? observers
          : receives;
    const page = witnesses.slice(start, start + size);
    return {
      label: `field ${field.toString()} walk ${start.toString()} (${size.toString()})`,
      role,
      next: state.complete ? EXHAUSTED[role] : role,
      datum: walkDatum(state),
      fields: [field],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                opening: open(field),
                checkpoint_bytes: plan(field).position("Walking", start)
                  .checkpointBytes,
                witnesses: [...page],
              },
            ],
          } as never,
          role === "allPurposesReceive"
            ? SDK.DirectAllPurposesReceiveRedeemer
            : SDK.DirectAllPurposesExecutionRedeemer,
        ),
    };
  };
  const pageAt = (
    role: ScriptIntegrityHashMismatchPagedRole,
    start: number,
    size: number,
  ): ScriptIntegrityHashMismatchPage => ({
    size,
    transitions:
      role === "allPurposesSpend"
        ? [take(start, size), resolve(start, size)]
        : [walkPage(role, start, size)],
  });
  const walkPages = (role: ScriptIntegrityHashMismatchPagedRole) => {
    const { field, batch: kind } = PAGED[role];
    for (const [start, size] of pages(counts(field), batch[kind]))
      out.push(...pageAt(role, start, size).transitions);
  };

  // Spend field: fixed stride, walking from the entry.
  let state = walking(0, 0);
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
  walkPages("allPurposesSpend");

  /** Certifies a variable field before it is walked. */
  const certify = (
    field: 5 | 2,
    label: string,
    role: "allPurposesMintGrammar" | "allPurposesReceiveGrammar",
    walkingRole: ScriptIntegrityHashMismatchTransition["next"],
    exhausted: ScriptIntegrityHashMismatchTransition["next"],
  ) => {
    state = {
      ...state,
      cursor: plan(field).position("Certifying", 0).cursor,
      complete: false,
    };
    out.push({
      label,
      role,
      next: role,
      datum: walkDatum(state),
      fields: [field],
      redeemer: ({ indices, open }) =>
        Data.to(
          { Continue: [{ Open: { ...indices, opening: open(field) } }] },
          SDK.DirectAllPurposesGrammarRedeemer,
        ),
    });
    const count = counts(field);
    for (const [start, budget] of grammarPages(count, batch.grammar)) {
      const last = start + budget >= count;
      state = last
        ? walking(field, 0)
        : {
            ...state,
            cursor: plan(field).position("Certifying", start + budget).cursor,
          };
      out.push({
        label: `field ${field.toString()} grammar ${start.toString()}`,
        role,
        next: last ? (count === 0 ? exhausted : walkingRole) : role,
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

  // Mint field: variable, certified before it is walked.
  state = walking(0, counts(0));
  certify(
    5,
    "mint open",
    "allPurposesMintGrammar",
    "allPurposesMint",
    "allPurposesObserveOpen",
  );
  walkPages("allPurposesMint");

  // Required observers: fixed stride, walking from their opening.
  state = walking(3, 0);
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
  walkPages("allPurposesObserve");

  // Receive outputs: variable, certified before they are walked.
  state = walking(3, counts(3));
  certify(
    2,
    "receive open",
    "allPurposesReceiveGrammar",
    "allPurposesReceive",
    "terminal",
  );
  walkPages("allPurposesReceive");
  const final = walking(2, counts(2));
  if (out.at(-1)!.next !== "terminal" || out.at(-1)!.datum !== walkDatum(final))
    throw new Error(`${LABEL}: the walk does not reach the terminal`);

  // A thread datum names its page by the walk checkpoint it holds.
  const checkpoints = new Map<number, ReadonlyMap<string, number>>();
  const walkedTo = (cursor: SDK.DirectFieldCursor, field: number): number => {
    if (cursor.field_index !== BigInt(field) || cursor.phase !== "Walking")
      throw new Error(
        `${LABEL}: thread datum does not walk field ${field.toString()}`,
      );
    let index = checkpoints.get(field);
    if (index === undefined) {
      index = new Map(
        Array.from({ length: counts(field) + 1 }, (_, at) => [
          plan(field)
            .position("Walking", at)
            .cursor.checkpoint_hash.toLowerCase(),
          at,
        ]),
      );
      checkpoints.set(field, index);
    }
    const at = index.get(cursor.checkpoint_hash.toLowerCase());
    if (at === undefined)
      throw new Error(`${LABEL}: thread datum names no walk checkpoint`);
    return at;
  };
  const same = (left: string, right: string) =>
    left.toLowerCase() === right.toLowerCase();
  const page: ScriptIntegrityHashMismatchWalk["page"] = (role, datum, size) => {
    const { field, batch: kind } = PAGED[role];
    const held = Data.from(datum, SDK.DirectAllPurposesDatum).data;
    if (held === null) throw new Error(`${LABEL}: thread datum holds no walk`);
    const start = walkedTo(held.cursor, field);
    if (!same(walkDatum(walking(field, start)), datum))
      throw new Error(`${LABEL}: thread datum differs from the planned walk`);
    const taken = Math.min(size ?? batch[kind], counts(field) - start);
    if (!Number.isSafeInteger(taken) || taken < 1)
      throw new Error(`${LABEL}: a ${role} page takes at least one item`);
    return pageAt(role, start, taken);
  };
  return {
    transitions: out,
    final,
    page,
    transitionAt: (role, datum) => {
      if (role !== "allPurposesSpendResolve")
        return page(role, datum).transitions[0]!;
      const held = Data.from(datum, SDK.DirectAllPurposesPendingDatum).data;
      if (held === null)
        throw new Error(`${LABEL}: thread datum holds no pending spends`);
      const size = held.pending.length;
      const start = walkedTo(held.state.cursor, 0) - size;
      if (size < 1 || start < 0 || !same(take(start, size).datum, datum))
        throw new Error(`${LABEL}: thread datum differs from the planned take`);
      return resolve(start, size);
    },
  };
};
