import type { ChainSyncEvent } from "@al-ft/l1-node-transport";

import { outRefKey } from "../codec.js";
import type { OutRef } from "../types.js";
import { type SimOutput, type SimTx, simTxHash } from "./block-cbor.js";
import type { Rng } from "./rng.js";
import type { SimChain, SimUtxo } from "./sim-chain.js";

/** The fork shapes the simulator covers (plan §15 F8, §16.1). */
export const FORK_SHAPES = [
  "reland",
  "never_reland",
  "changed_valid_to",
  "new_fork_only",
  "phase2_failed",
] as const;

export type ForkShape = (typeof FORK_SHAPES)[number];

/**
 * One fork episode: `lead` plain blocks, a funding block creating the
 * subject's tracked input F and tracked collateral C, an old branch of
 * `depth` blocks (the subject transaction, if the shape has one there, in
 * its first block), a rollback of `depth`, then a new branch of
 * `depth + extra` blocks with the shape's transaction at `landAt` (modulo
 * the branch length).
 *
 * - `reland`: the same transaction lands again.
 * - `never_reland`: it does not; with `variant % 2 === 1` a conflicting
 *   transaction spends F instead, so it never can.
 * - `changed_valid_to`: the transaction with a different upper validity
 *   bound (a new id, the same inputs) lands instead.
 * - `new_fork_only`: nothing in the old branch; a transaction spending F
 *   lands only on the new branch.
 * - `phase2_failed`: the old transaction failed phase 2 (C consumed, its
 *   collateral return created). `variant % 3`: 0 it lands failed again,
 *   1 a valid replacement spends F instead, 2 nothing replaces it.
 */
export type ForkEpisode = Readonly<{
  shape: ForkShape;
  depth: number;
  extra: number;
  landAt: number;
  variant: number;
  lead: number;
}>;

export type ForkScenario = Readonly<{
  /** Seeds the filler traffic. */
  seed: number;
  episodes: readonly ForkEpisode[];
}>;

/** A fact the model says must hold in the store at a checkpoint. */
export type ForkCheck =
  | Readonly<{
      kind: "tx_present";
      what: string;
      hash: Buffer;
      isValid: boolean;
      invalidAfter: number | null;
    }>
  | Readonly<{ kind: "tx_absent"; what: string; hash: Buffer }>
  | Readonly<{
      kind: "spender";
      what: string;
      outRef: OutRef;
      /** Null: the output must be live. */
      spentBy: Buffer | null;
    }>;

export type ForkCheckpoint = Readonly<{
  label: string;
  checks: readonly ForkCheck[];
  /** The model's live tracked outrefs (68-hex keys, sorted). */
  liveTracked: readonly string[];
}>;

export type ForkStep = Readonly<{
  event: ChainSyncEvent;
  checkpoint?: ForkCheckpoint;
}>;

/**
 * Extra transactions for one block (a projection's own traffic, added after
 * the filler). Before spending a live outref it must `claim` it: false means
 * the outref is an episode's subject input or already spent in this block.
 */
export type ScenarioTraffic = (
  context: Readonly<{
    chain: SimChain;
    rng: Rng;
    claim: (outRef: OutRef) => boolean;
  }>,
) => SimTx[];

type Block = {
  txs: SimTx[];
  used: Set<string>;
  pending: SimUtxo[];
};

const outputFor = (chain: SimChain, rng: Rng): SimOutput => {
  const u = chain.universe;
  const roll = rng.int(4);
  const address =
    roll === 0
      ? u.trackedAddress
      : roll === 1
        ? u.credentialAddress
        : u.untrackedAddress;
  return {
    address,
    lovelace: BigInt(rng.range(1, 40)) * 1_000_000n,
    ...(rng.chance(0.2)
      ? { datum: Buffer.from([0xd8, 0x79, 0x9f, rng.int(24), 0xff]) }
      : {}),
  };
};

const pickInput = (
  chain: SimChain,
  rng: Rng,
  block: Block,
  reserved: ReadonlySet<string>,
): OutRef => {
  const candidates = [...chain.live(), ...block.pending].filter((utxo) => {
    const key = outRefKey(utxo.outRef);
    return !reserved.has(key) && !block.used.has(key);
  });
  if (candidates.length === 0 || rng.chance(0.3)) return chain.outsideInput();
  const tracked = candidates.filter((utxo) => chain.isTracked(utxo.output));
  const utxo =
    tracked.length > 0 && rng.chance(0.6)
      ? rng.pick(tracked)
      : rng.pick(candidates);
  block.used.add(outRefKey(utxo.outRef));
  return utxo.outRef;
};

const push = (block: Block, tx: SimTx): void => {
  block.txs.push(tx);
  const hash = simTxHash(tx);
  if (tx.isValid === false) {
    if (tx.collateralReturn !== undefined)
      block.pending.push({
        outRef: { txHash: hash, index: tx.outputs.length },
        output: tx.collateralReturn,
      });
    return;
  }
  tx.outputs.forEach((output, index) =>
    block.pending.push({ outRef: { txHash: hash, index }, output }),
  );
};

/** Random traffic over the model's live UTxOs, avoiding `reserved`. */
const fillerTx = (
  chain: SimChain,
  rng: Rng,
  block: Block,
  reserved: ReadonlySet<string>,
): SimTx => {
  const u = chain.universe;
  const inputs = [pickInput(chain, rng, block, reserved)];
  if (rng.chance(0.3)) inputs.push(pickInput(chain, rng, block, reserved));
  const unique = [...new Map(inputs.map((i) => [outRefKey(i), i])).values()];
  const outputs = Array.from({ length: rng.range(1, 2) }, () =>
    outputFor(chain, rng),
  );
  const failed = rng.chance(0.1);
  const mint = rng.chance(0.15)
    ? new Map([
        [
          rng.chance(0.5) ? u.trackedPolicy : u.untrackedPolicy,
          new Map([["aa", BigInt(rng.range(1, 9))]]),
        ],
      ])
    : undefined;
  const reference =
    rng.chance(0.2) && chain.live().length > 0
      ? chain
          .live()
          .filter(
            (utxo) =>
              !block.used.has(outRefKey(utxo.outRef)) &&
              !reserved.has(outRefKey(utxo.outRef)),
          )
          .slice(0, 1)
          .map((utxo) => utxo.outRef)
      : [];
  return {
    inputs: unique,
    outputs,
    ...(mint === undefined ? {} : { mint }),
    ...(reference.length > 0 ? { referenceInputs: reference } : {}),
    ...(failed
      ? {
          isValid: false,
          collaterals: [pickInput(chain, rng, block, reserved)],
          ...(rng.chance(0.7)
            ? { collateralReturn: outputFor(chain, rng) }
            : {}),
        }
      : {}),
    nonce: chain.nonce(),
  };
};

/** Builds the event sequence of a scenario on `chain`, with checkpoints. */
export class EpisodeBuilder {
  readonly steps: ForkStep[] = [];
  private readonly reserved = new Set<string>();

  constructor(
    readonly chain: SimChain,
    private readonly rng: Rng,
    private readonly traffic: readonly ScenarioTraffic[] = [],
  ) {}

  /** One block: `subject` first (if any), then filler and projection traffic. */
  block(subject: readonly SimTx[] = []): void {
    const block: Block = { txs: [], used: new Set(), pending: [] };
    for (const tx of subject) push(block, tx);
    for (let i = this.rng.int(3); i > 0; i -= 1)
      push(block, fillerTx(this.chain, this.rng, block, this.reserved));
    const claim = (outRef: OutRef): boolean => {
      const key = outRefKey(outRef);
      if (this.reserved.has(key) || block.used.has(key)) return false;
      block.used.add(key);
      return true;
    };
    for (const extra of this.traffic)
      for (const tx of extra({ chain: this.chain, rng: this.rng, claim }))
        push(block, tx);
    this.steps.push({ event: this.chain.forward(block.txs).event });
  }

  private checkpoint(label: string, checks: readonly ForkCheck[]): void {
    const last = this.steps.pop();
    if (last === undefined) throw new Error("checkpoint before any event");
    this.steps.push({
      event: last.event,
      checkpoint: { label, checks, liveTracked: this.chain.liveTracked() },
    });
  }

  episode(episode: ForkEpisode, index: number): void {
    const { chain } = this;
    const u = chain.universe;
    const label = `episode ${index} (${episode.shape}, depth ${episode.depth})`;
    for (let i = 0; i < episode.lead; i += 1) this.block();
    const funding: SimTx = {
      inputs: [chain.outsideInput()],
      outputs: [
        { address: u.trackedAddress, lovelace: 50_000_000n },
        { address: u.credentialAddress, lovelace: 5_000_000n },
        { address: u.untrackedAddress, lovelace: 1_000_000n },
      ],
      nonce: chain.nonce(),
    };
    const fundingHash = simTxHash(funding);
    const f = { txHash: fundingHash, index: 0 };
    const c = { txHash: fundingHash, index: 1 };
    this.reserved.add(outRefKey(f));
    this.reserved.add(outRefKey(c));
    this.block([funding]);
    const validTo = chain.tip.point.slot + 500;
    const spend = (
      outputs: SimOutput[],
      fields: Partial<SimTx> = {},
    ): SimTx => ({
      inputs: [f],
      outputs,
      collaterals: [c],
      invalidAfter: validTo,
      nonce: chain.nonce(),
      ...fields,
    });
    const trackedOut = { address: u.trackedAddress, lovelace: 20_000_000n };
    const credentialOut = {
      address: u.credentialAddress,
      lovelace: 3_000_000n,
    };
    const old: SimTx | undefined =
      episode.shape === "new_fork_only"
        ? undefined
        : episode.shape === "phase2_failed"
          ? spend([trackedOut], {
              isValid: false,
              collateralReturn: credentialOut,
            })
          : spend([trackedOut, { address: u.untrackedAddress, lovelace: 1n }]);
    for (let i = 0; i < episode.depth; i += 1)
      this.block(i === 0 && old !== undefined ? [old] : []);
    this.steps.push({ event: chain.backward(episode.depth) });
    const oldHash = old === undefined ? null : simTxHash(old);
    this.checkpoint(`${label}: after the rollback`, [
      ...(oldHash === null
        ? []
        : [{ kind: "tx_absent" as const, what: "old tx", hash: oldHash }]),
      { kind: "spender", what: "F", outRef: f, spentBy: null },
      { kind: "spender", what: "C", outRef: c, spentBy: null },
    ]);
    const { landing, checks } = this.newBranchSubject(episode, old, spend, {
      f,
      c,
      trackedOut,
      credentialOut,
    });
    const length = episode.depth + episode.extra;
    const at = episode.landAt % length;
    for (let i = 0; i < length; i += 1)
      this.block(i === at && landing !== undefined ? [landing] : []);
    this.checkpoint(`${label}: end of the new branch`, checks);
    this.reserved.delete(outRefKey(f));
    this.reserved.delete(outRefKey(c));
  }

  private newBranchSubject(
    episode: ForkEpisode,
    old: SimTx | undefined,
    spend: (outputs: SimOutput[], fields?: Partial<SimTx>) => SimTx,
    io: Readonly<{
      f: OutRef;
      c: OutRef;
      trackedOut: SimOutput;
      credentialOut: SimOutput;
    }>,
  ): { landing: SimTx | undefined; checks: ForkCheck[] } {
    const { f, c } = io;
    const present = (what: string, tx: SimTx): ForkCheck => ({
      kind: "tx_present",
      what,
      hash: simTxHash(tx),
      isValid: tx.isValid !== false,
      invalidAfter: tx.invalidAfter ?? null,
    });
    const absent = (what: string, tx: SimTx): ForkCheck => ({
      kind: "tx_absent",
      what,
      hash: simTxHash(tx),
    });
    const spent = (what: string, outRef: OutRef, by: SimTx | null) =>
      ({
        kind: "spender",
        what,
        outRef,
        spentBy: by === null ? null : simTxHash(by),
      }) satisfies ForkCheck;
    switch (episode.shape) {
      case "reland": {
        const tx = old as SimTx;
        return {
          landing: tx,
          checks: [present("re-landed tx", tx), spent("F", f, tx)],
        };
      }
      case "never_reland": {
        const tx = old as SimTx;
        const conflict =
          episode.variant % 2 === 1
            ? spend([{ ...io.credentialOut, lovelace: 4_000_000n }])
            : undefined;
        return {
          landing: conflict,
          checks: [
            absent("never re-landed tx", tx),
            ...(conflict === undefined ? [] : [present("conflict", conflict)]),
            spent("F", f, conflict ?? null),
          ],
        };
      }
      case "changed_valid_to": {
        const tx = old as SimTx;
        const changed: SimTx = {
          ...tx,
          invalidAfter: (tx.invalidAfter ?? 0) + 1 + episode.variant,
        };
        return {
          landing: changed,
          checks: [
            absent("old-valid_to tx", tx),
            present("new-valid_to tx", changed),
            spent("F", f, changed),
          ],
        };
      }
      case "new_fork_only": {
        const tx = spend([io.credentialOut]);
        return {
          landing: tx,
          checks: [present("new-fork-only tx", tx), spent("F", f, tx)],
        };
      }
      case "phase2_failed": {
        const tx = old as SimTx;
        const outcome = episode.variant % 3;
        if (outcome === 0)
          return {
            landing: tx,
            checks: [
              present("re-landed failed tx", tx),
              spent("C", c, tx),
              spent("F", f, null),
            ],
          };
        if (outcome === 1) {
          const replacement = spend([io.trackedOut]);
          return {
            landing: replacement,
            checks: [
              absent("failed tx", tx),
              present("valid replacement", replacement),
              spent("F", f, replacement),
              spent("C", c, null),
            ],
          };
        }
        return {
          landing: undefined,
          checks: [
            absent("failed tx", tx),
            spent("F", f, null),
            spent("C", c, null),
          ],
        };
      }
    }
  }
}
