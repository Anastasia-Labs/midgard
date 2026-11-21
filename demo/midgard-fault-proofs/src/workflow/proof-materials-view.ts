/**
 * One view of the prover's proof-materials address for the whole process.
 *
 * Every family publishes the data its later steps read (proof chunks, field
 * carriage, transition-trace carriage, staged script-decode bytes, CEK
 * operand staging) as plain outputs at the proof-materials address. The
 * chain checks only their content, so an output is found by content: its
 * exact inline datum bytes, with nothing but ada, no datum hash and no
 * reference script. A copy of another shape is never used and never swept:
 * a step referencing it would pay for its reference script, and a sweep
 * spending it would carry its token. Copies are taken in out-ref order, so
 * every workflow of the process picks the same one, and one output serves
 * every workflow that needs those bytes.
 *
 * The view also keeps, in memory only, what each live workflow needs (its
 * datums and the out-refs it has pinned), rebuilt by each workflow's
 * observation. A sweep takes only plain outputs no live workflow needs, and
 * only once
 * every live objective of the process has completed an observation since
 * start; before that the needs are not all known. Nothing here is persisted:
 * after a restart the needs come back from each workflow's next observation.
 */
import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

/** Plain outputs read in one snapshot, with that snapshot's point if any. */
export type ProofMaterialsRead = Readonly<{
  /**
   * The point of the snapshot: the ledger point and the mempool
   * transactions applied over it; none for a read without one.
   */
  point?: string;
  utxos: readonly UTxO[];
}>;

/**
 * How the view reads the address. Out-refs are `txHash#index` labels and
 * datums lowercase hex of the exact inline datum bytes.
 */
export type ProofMaterialsReader = Readonly<{
  /**
   * For each request, the first `limit` plain outputs whose inline datum is
   * exactly `datum`, in out-ref order.
   */
  byDatum: (
    requests: readonly Readonly<{ datum: string; limit: number }>[],
  ) => Promise<ProofMaterialsRead>;
  /**
   * The first `limit` plain outputs in out-ref order whose inline datum, if
   * any, is none of `datums`, skipping `excluded`.
   */
  exceptDatums: (
    datums: readonly string[],
    excluded: readonly string[],
    limit: number,
  ) => Promise<ProofMaterialsRead>;
  /** The point a read now would be at; without one nothing is memoised. */
  point?: () => Promise<string>;
}>;

/** An objective of the process: one category on one header. */
export type ProofMaterialsObjective = Readonly<{
  category: string;
  headerHash: string;
}>;

/** The workflow a need belongs to, and the objective that workflow serves. */
export type ProofMaterialsHolder = Readonly<{
  workflowId: string;
  objective: ProofMaterialsObjective;
}>;

/** A view and the workflow that finds through it. */
export type ProofMaterialsAccess = Readonly<{
  view: ProofMaterialsView;
  holder: ProofMaterialsHolder;
}>;

/** Whether a sweep may select now, and why not when it may not. */
export type ProofMaterialsSweepReadiness =
  | Readonly<{ open: true }>
  | Readonly<{ open: false; reason: string }>;

/** Why a sweep is closed before the process knows its live objectives. */
export const PROOF_MATERIALS_SWEEP_FIRST_PASS_PENDING =
  "the first admission pass at the follower tip has not completed in this process";

export type ProofMaterialsView = Readonly<{
  /** The proof-materials address this view reads. */
  address: string;
  /**
   * For each requested datum, in request order, a live plain output with
   * exactly those bytes, or undefined: the first copy for its first
   * occurrence in `datums`, the second for its second, and so on. The
   * datums are registered as needs of `holder` before the read, so no sweep
   * takes what a step found.
   */
  find: (
    datums: readonly string[],
    holder: ProofMaterialsHolder,
  ) => Promise<readonly (UTxO | undefined)[]>;
  /** Adds to what `holder` needs until it is released. */
  need: (
    holder: ProofMaterialsHolder,
    datums: readonly string[],
    pinnedOutRefs?: readonly string[],
  ) => void;
  /** Drops everything workflow `workflowId` registered. */
  release: (workflowId: string) => void;
  /** Drops everything every workflow of `objective` registered. */
  releaseObjective: (objective: ProofMaterialsObjective) => void;
  /** Records that `objective` completed an observation in this process. */
  observed: (objective: ProofMaterialsObjective) => void;
  /** Whether a sweep may select now; when not, the first reason. */
  readiness: () => ProofMaterialsSweepReadiness;
  /**
   * Up to `limit` plain outputs no registered need or pin names, in
   * out-ref order; none while `readiness` is closed.
   */
  sweepable: (limit: number) => Promise<readonly UTxO[]>;
  /** The outputs of `page` no need or pin names now. */
  unneeded: (page: readonly UTxO[]) => readonly UTxO[];
}>;

const OBJECTIVE_SEPARATOR = "\u0000";
const objectiveKey = ({ category, headerHash }: ProofMaterialsObjective) =>
  `${category}${OBJECTIVE_SEPARATOR}${headerHash.toLowerCase()}`;

const utxoLabel = (utxo: UTxO): string =>
  outRefLabel({ txHash: utxo.txHash, outputIndex: utxo.outputIndex });

const byOutRef = (left: UTxO, right: UTxO): number =>
  compareOutRefs(left, right);

/**
 * Whether `utxo` has the strict shape of a proof material: only ada, no
 * datum hash and no reference script. Only such an output is found or
 * swept; its inline datum, if any, is free.
 */
export const isPlainProofMaterialsOutput = (utxo: UTxO): boolean =>
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
  );

const datumKey = (datum: string): string => {
  if (!/^(?:[0-9a-fA-F]{2})+$/u.test(datum))
    throw new TypeError("a proof-materials datum must be non-empty hex bytes");
  return datum.toLowerCase();
};

/**
 * The reader over `lucid.utxosAt(address)` (a Lucid, or anything that reads
 * an address the same way) with the strict shape checked in memory, for a
 * caller without a follower. It has no point: nothing is memoised.
 */
export const lucidProofMaterialsReader = (
  lucid: Pick<LucidEvolution, "utxosAt">,
  address: string,
): ProofMaterialsReader => {
  const plain = async (excluded: readonly string[]) => {
    const skip = new Set(excluded);
    return (await lucid.utxosAt(address))
      .filter(
        (utxo) =>
          utxo.address === address &&
          isPlainProofMaterialsOutput(utxo) &&
          !skip.has(utxoLabel(utxo)),
      )
      .sort(byOutRef);
  };
  return Object.freeze({
    byDatum: async (requests) => {
      const limits = new Map<string, number>();
      for (const { datum, limit } of requests)
        limits.set(datum, Math.max(limits.get(datum) ?? 0, limit));
      const taken = new Map<string, number>();
      const utxos = (await plain([])).filter((utxo) => {
        if (typeof utxo.datum !== "string") return false;
        const datum = utxo.datum.toLowerCase();
        const count = taken.get(datum) ?? 0;
        if (count >= (limits.get(datum) ?? 0)) return false;
        taken.set(datum, count + 1);
        return true;
      });
      return { utxos };
    },
    exceptDatums: async (datums, excluded, limit) => {
      const needed = new Set(datums);
      return {
        utxos: (await plain(excluded))
          .filter(
            (utxo) =>
              typeof utxo.datum !== "string" ||
              !needed.has(utxo.datum.toLowerCase()),
          )
          .slice(0, limit),
      };
    },
  });
};

type Need = { objective: string; datums: Set<string>; pins: Set<string> };

/**
 * The view of `address` over `reader`. `liveObjectives` lists the live
 * objectives of the process, undefined while they are not yet known; a
 * process without the listing (one workflow, run by itself) sweeps when
 * the sweeping workflow does, after its own observation. Build one per
 * prover key per process and share it with every workflow: the needs only
 * protect what they share.
 */
export const createProofMaterialsView = ({
  address,
  reader,
  liveObjectives,
}: {
  readonly address: string;
  readonly reader: ProofMaterialsReader;
  readonly liveObjectives?: () =>
    | readonly ProofMaterialsObjective[]
    | undefined;
}): ProofMaterialsView => {
  const needs = new Map<string, Need>();
  const observedObjectives = new Set<string>();
  // Reads at one point: dropped as soon as the ledger or the mempool moves.
  let memo: { point: string; reads: Map<string, ProofMaterialsRead> } = {
    point: "",
    reads: new Map(),
  };

  const neededDatums = (): string[] =>
    [
      ...new Set([...needs.values()].flatMap(({ datums }) => [...datums])),
    ].sort();
  const pinned = (): Set<string> =>
    new Set([...needs.values()].flatMap(({ pins }) => [...pins]));

  const memoised = async (
    key: string,
    read: () => Promise<ProofMaterialsRead>,
  ): Promise<ProofMaterialsRead> => {
    const point = await reader.point?.();
    if (point === undefined) return await read();
    if (memo.point !== point) memo = { point, reads: new Map() };
    const seen = memo.reads.get(key);
    if (seen !== undefined) return seen;
    const result = await read();
    if (result.point === point) memo.reads.set(key, result);
    return result;
  };

  const readiness = (): ProofMaterialsSweepReadiness => {
    if (liveObjectives === undefined) return { open: true };
    const live = liveObjectives();
    if (live === undefined)
      return { open: false, reason: PROOF_MATERIALS_SWEEP_FIRST_PASS_PENDING };
    const waiting = live.find(
      (objective) => !observedObjectives.has(objectiveKey(objective)),
    );
    return waiting === undefined
      ? { open: true }
      : {
          open: false,
          reason: `${waiting.category}/${waiting.headerHash.toLowerCase()} has not observed since this process started`,
        };
  };

  const register = (
    { workflowId, objective }: ProofMaterialsHolder,
    datums: readonly string[],
    pinnedOutRefs: readonly string[],
  ): void => {
    if (workflowId.length === 0)
      throw new TypeError("a proof-materials need names its workflow");
    const key = objectiveKey(objective);
    const entry = needs.get(workflowId) ?? {
      objective: key,
      datums: new Set<string>(),
      pins: new Set<string>(),
    };
    if (entry.objective !== key)
      throw new Error(
        `workflow ${workflowId} registered proof materials for another objective`,
      );
    for (const datum of datums) entry.datums.add(datumKey(datum));
    for (const outRef of pinnedOutRefs) entry.pins.add(outRef);
    needs.set(workflowId, entry);
  };

  return Object.freeze({
    address,
    find: async (datums, holder) => {
      const keys = datums.map(datumKey);
      register(holder, keys, []);
      if (keys.length === 0) return [];
      const counts = new Map<string, number>();
      for (const key of keys) counts.set(key, (counts.get(key) ?? 0) + 1);
      const requests = [...counts]
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([datum, limit]) => ({ datum, limit }));
      const read = await memoised(
        JSON.stringify(["find", requests]),
        async () => await reader.byDatum(requests),
      );
      const copies = new Map<string, UTxO[]>();
      for (const utxo of [...read.utxos].sort(byOutRef)) {
        if (
          utxo.address !== address ||
          !isPlainProofMaterialsOutput(utxo) ||
          typeof utxo.datum !== "string"
        )
          continue;
        const key = utxo.datum.toLowerCase();
        copies.set(key, [...(copies.get(key) ?? []), utxo]);
      }
      return Object.freeze(keys.map((key) => copies.get(key)?.shift()));
    },
    need: (holder, datums, pinnedOutRefs = []) => {
      register(holder, datums, pinnedOutRefs);
    },
    release: (workflowId) => {
      needs.delete(workflowId);
    },
    releaseObjective: (objective) => {
      const key = objectiveKey(objective);
      for (const [workflowId, entry] of needs)
        if (entry.objective === key) needs.delete(workflowId);
    },
    observed: (objective) => {
      observedObjectives.add(objectiveKey(objective));
    },
    readiness,
    sweepable: async (limit) => {
      if (!Number.isSafeInteger(limit) || limit < 1)
        throw new RangeError("a sweep page limit must be a positive integer");
      if (!readiness().open) return [];
      const datums = neededDatums();
      const pins = pinned();
      const skip = [...pins].sort();
      const read = await memoised(
        JSON.stringify(["sweepable", datums, skip, limit]),
        async () => await reader.exceptDatums(datums, skip, limit),
      );
      const needed = new Set(datums);
      return Object.freeze(
        [...read.utxos]
          .filter(
            (utxo) =>
              utxo.address === address &&
              isPlainProofMaterialsOutput(utxo) &&
              !pins.has(utxoLabel(utxo)) &&
              (typeof utxo.datum !== "string" ||
                !needed.has(utxo.datum.toLowerCase())),
          )
          .sort(byOutRef)
          .slice(0, limit),
      );
    },
    unneeded: (page) => {
      const needed = new Set(neededDatums());
      const pins = pinned();
      return Object.freeze(
        page.filter(
          (utxo) =>
            utxo.address === address &&
            isPlainProofMaterialsOutput(utxo) &&
            !pins.has(utxoLabel(utxo)) &&
            (typeof utxo.datum !== "string" ||
              !needed.has(utxo.datum.toLowerCase())),
        ),
      );
    },
  });
};

const defaultViews = new WeakMap<
  LucidEvolution,
  Map<string, ProofMaterialsView>
>();

/**
 * `view`, or when the caller supplied none the view of `address` over
 * `lucid`, one per Lucid instance and address: a process that runs its
 * workflows each on its own Lucid shares none of their needs, so a process
 * running several workflows on one chain passes one shared view instead.
 */
export const proofMaterialsViewFor = ({
  view,
  lucid,
  address,
}: {
  readonly view: ProofMaterialsView | undefined;
  readonly lucid: LucidEvolution;
  readonly address: string;
}): ProofMaterialsView => {
  if (view !== undefined) {
    if (view.address !== address)
      throw new Error(
        `the proof-materials view reads ${view.address}, not ${address}`,
      );
    return view;
  }
  let views = defaultViews.get(lucid);
  if (views === undefined) {
    views = new Map();
    defaultViews.set(lucid, views);
  }
  let created = views.get(address);
  if (created === undefined) {
    created = createProofMaterialsView({
      address,
      reader: lucidProofMaterialsReader(lucid, address),
    });
    views.set(address, created);
  }
  return created;
};

/**
 * What `holder` finds through: the signer's view of its proof-materials
 * address, or the default view over `lucid` when it carries none.
 */
export const proofMaterialsAccess = ({
  signer,
  lucid,
  holder,
}: {
  readonly signer: Readonly<{
    proofMaterialsAddress: string;
    proofMaterials?: ProofMaterialsView;
  }>;
  readonly lucid: LucidEvolution;
  readonly holder: ProofMaterialsHolder;
}): ProofMaterialsAccess =>
  Object.freeze({
    view: proofMaterialsViewFor({
      view: signer.proofMaterials,
      lucid,
      address: signer.proofMaterialsAddress,
    }),
    holder,
  });
