/** The planned custody of a direct purpose thread with every proof-opening
 * run re-pageable: a thread at a page boundary continues with a page of any
 * size, and a thread inside a page of any size finds its next transition from
 * the datum it holds. */
import type { FittedPage } from "../page-fit.js";
import type {
  DirectPurposePagedRun,
  DirectPurposeTransition,
} from "./custody.js";
import type { DirectPurposeRole } from "./family.js";

export type DirectPurposePaging = Readonly<{
  /** Whether a thread holding `datum` at `role` is at a page boundary. */
  isPageStart: (role: DirectPurposeRole, datum: string) => boolean;
  /** The page continuing a thread at a page boundary, of `size` items (the
   * run's configured maximum by default), never more than remain. */
  page: (
    role: DirectPurposeRole,
    datum: string,
    size?: number,
  ) => FittedPage<DirectPurposeTransition>;
  /** The one transition continuing a thread holding `datum` at `role`: the
   * configured page at a page boundary, the next transition of a page taken
   * at any size inside one, the planned transition elsewhere. */
  transitionAt: (
    role: DirectPurposeRole,
    datum: string,
  ) => DirectPurposeTransition;
}>;

/** Page sizes a fit takes: the maximum, halved down to one. */
const halvings = (batch: number): number[] => {
  const sizes: number[] = [];
  for (let size = batch; size >= 1; size = Math.floor(size / 2))
    sizes.push(size);
  return sizes;
};

const same = (left: string, right: string) =>
  left.toLowerCase() === right.toLowerCase();

export const directPurposePaging = ({
  label,
  transitions,
  first,
}: {
  readonly label: string;
  /** The planned custody, its runs at their configured page sizes. */
  readonly transitions: readonly DirectPurposeTransition[];
  /** The datum the first planned transition continues. */
  readonly first: string;
}): DirectPurposePaging => {
  const runs = [
    ...new Set(transitions.flatMap(({ paged }) => paged?.run ?? [])),
  ];
  const boundaries = new Map<DirectPurposePagedRun, Map<string, number>>();
  const boundariesOf = (run: DirectPurposePagedRun) => {
    let found = boundaries.get(run);
    if (found === undefined) {
      found = new Map(
        Array.from({ length: run.count }, (_, start) => [
          run.datumAt(start).toLowerCase(),
          start,
        ]),
      );
      boundaries.set(run, found);
    }
    return found;
  };
  const boundaryAt = (role: DirectPurposeRole, datum: string) => {
    for (const run of runs) {
      if (run.role !== role) continue;
      const start = boundariesOf(run).get(datum.toLowerCase());
      if (start !== undefined) return { run, start };
    }
    return null;
  };
  // Transitions of pages taken, by the role and datum they continue.
  const taken = new Map<string, DirectPurposeTransition>();
  const key = (role: DirectPurposeRole, datum: string) =>
    `${role}\n${datum.toLowerCase()}`;
  const take = (run: DirectPurposePagedRun, start: number, size: number) => {
    const page = run.page(start, size);
    let previous = run.datumAt(start);
    for (const transition of page) {
      taken.set(key(transition.role, previous), transition);
      previous = transition.datum;
    }
    return page;
  };
  const page: DirectPurposePaging["page"] = (role, datum, size) => {
    const boundary = boundaryAt(role, datum);
    if (boundary === null)
      throw new Error(`${label} ${role} thread is not at a page boundary`);
    const { run, start } = boundary;
    const size_ = Math.min(size ?? run.batch, run.count - start);
    if (!Number.isSafeInteger(size_) || size_ < 1)
      throw new Error(`${label}: a ${role} page takes at least one item`);
    return { size: size_, transitions: take(run, start, size_) };
  };
  const planned = (role: DirectPurposeRole, datum: string) =>
    transitions.filter(
      (transition, index) =>
        transition.role === role &&
        same(index === 0 ? first : transitions[index - 1]!.datum, datum),
    );
  /** A page taken at a fitted size before this plan was built. */
  const search = (role: DirectPurposeRole, datum: string) => {
    for (const run of runs)
      for (let start = 0; start < run.count; start += 1)
        for (const size of halvings(Math.min(run.batch, run.count - start))) {
          take(run, start, size);
          const found = taken.get(key(role, datum));
          if (found !== undefined) return found;
        }
    return undefined;
  };
  return Object.freeze({
    isPageStart: (role, datum) => boundaryAt(role, datum) !== null,
    page,
    transitionAt: (role, datum) => {
      if (boundaryAt(role, datum) !== null)
        return page(role, datum).transitions[0]!;
      const remembered = taken.get(key(role, datum));
      if (remembered !== undefined) return remembered;
      const matches = planned(role, datum);
      if (matches.length === 1) return matches[0]!;
      const found = matches.length === 0 ? search(role, datum) : undefined;
      if (found !== undefined) return found;
      throw new Error(
        `${label} ${role} thread datum matches ${matches.length.toString()} planned transitions`,
      );
    },
  });
};
