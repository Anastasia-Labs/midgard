import {
  hashMidgardValidationMachineState,
  type MidgardValidationDispute,
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import {
  requireInputIndex,
  ValidationBoundarySpendRedeemer,
  ValidationGameSpendRedeemer,
  validationMachineStateDataFromCore,
  ValidationResolutionDatum,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";

import {
  type Funding,
  type GameScene,
  type Move,
  scriptAddress,
  type Thread,
  threadDatum,
} from "./validation-game-scene.js";

type Indices = { readonly input_index: bigint; readonly output_index: bigint };

export type ThreadMoveArgs = {
  readonly scene: GameScene;
  readonly thread: UTxO;
  readonly script: Script;
  readonly redeem: (indices: Indices) => string;
  readonly toAddress: string;
  readonly toDatum: string;
  readonly funding: Funding;
  readonly signed: boolean;
  readonly validFrom: number;
  readonly validTo: number;
  /** The successor's output index; the honest layout puts it first. */
  readonly outputIndex?: bigint;
};

/** One thread carried to its successor at output 0, keeping its value. */
export const threadMove = (args: ThreadMoveArgs): Move => ({
  spends: [
    {
      utxo: args.thread,
      script: args.script,
      redeemer: (ctx) =>
        args.redeem({
          input_index: requireInputIndex(ctx, args.thread, "thread move"),
          output_index: args.outputIndex ?? 0n,
        }),
    },
  ],
  outputs: [
    {
      address: args.toAddress,
      datum: args.toDatum,
      assets: { ...args.thread.assets },
    },
  ],
  funding: args.funding,
  signers: args.signed ? [args.scene.proverKey] : [],
  validFrom: args.validFrom,
  validTo: args.validTo,
});

export type GameAction =
  | {
      readonly kind: "RevealOperator" | "RevealChallenger";
      readonly proof: MidgardValidationTraceProof;
    }
  | { readonly kind: "EnterResolution" | "EnterChallengerTimeout" };

export const gameRedeemer = (action: GameAction) => (indices: Indices) =>
  Data.to(
    {
      Continue: [
        action.kind === "RevealOperator"
          ? {
              RevealOperator: {
                ...indices,
                proof: validationTraceProofDataFromCore(action.proof),
              },
            }
          : action.kind === "RevealChallenger"
            ? {
                RevealChallenger: {
                  ...indices,
                  proof: validationTraceProofDataFromCore(action.proof),
                },
              }
            : action.kind === "EnterResolution"
              ? { EnterResolution: indices }
              : { EnterChallengerTimeout: indices },
      ],
    },
    ValidationGameSpendRedeemer,
  );

export type MoveOptions = {
  readonly funding: Funding;
  readonly signed: boolean;
  readonly validFrom: number;
  readonly validTo: number;
  /** The successor's output index; the honest layout puts it first. */
  readonly outputIndex?: bigint;
};

/** A game move from `thread` to the game state `next`. */
export const gameMove = (
  scene: GameScene,
  thread: Pick<Thread, "utxo" | "identity">,
  action: GameAction,
  next: MidgardValidationDispute,
  options: MoveOptions,
): Move =>
  threadMove({
    scene,
    thread: thread.utxo,
    script: scene.game.script,
    redeem: gameRedeemer(action),
    toAddress:
      action.kind === "EnterResolution"
        ? scene.boundary.address
        : action.kind === "EnterChallengerTimeout"
          ? scene.timeout.address
          : scene.game.address,
    toDatum: threadDatum(scene, thread.identity, next),
    ...options,
  });

export const RESOLVER_INDEX = {
  canonicalDecode: 0,
  compactBinding: 1,
  staticLedgerRules: 2,
  inputSets: 3,
  signatures: 4,
  phaseANativeScripts: 5,
  phaseAScriptPreconditions: 6,
  resolveInputs: 7,
  scriptSources: 8,
  nativeScripts: 9,
  scriptIntegrity: 10,
  cek: 11,
  valueAndMint: 12,
  ledgerDelta: 13,
} as const;

/** PrepareResolution on a boundary thread, routed to the phase's resolver. */
export const prepareResolutionMove = (
  scene: GameScene,
  thread: Thread,
  preState: MidgardValidationMachineState,
  options: MoveOptions,
): Move => {
  if (preState.phase === "terminal") {
    throw new Error("a terminal pre-state has no resolver");
  }
  const resolverIndex = RESOLVER_INDEX[preState.phase];
  return threadMove({
    scene,
    thread: thread.utxo,
    script: scene.boundary.script,
    redeem: (indices) =>
      Data.to(
        {
          Continue: [
            {
              PrepareResolution: {
                ...indices,
                resolver_index: BigInt(resolverIndex),
                evidence: {
                  pre_state: validationMachineStateDataFromCore(preState),
                },
              },
            },
          ],
        },
        ValidationBoundarySpendRedeemer,
      ),
    toAddress: scriptAddress(scene.resolverHashes[resolverIndex]!),
    toDatum: Data.to(
      {
        fraud_prover: scene.proverKey,
        data: {
          version: 1n,
          pre_state: validationMachineStateDataFromCore(preState),
          operator_successor_hash:
            thread.dispute.operatorHighHash.toString("hex"),
          challenger_successor_hash:
            thread.dispute.challengerHighHash.toString("hex"),
        },
      },
      ValidationResolutionDatum,
    ),
    ...options,
  });
};

export const stateHashHex = (state: MidgardValidationMachineState) =>
  hashMidgardValidationMachineState(state).toString("hex");
