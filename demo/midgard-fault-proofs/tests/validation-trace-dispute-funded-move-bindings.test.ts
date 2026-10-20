/** The prover funding permit's thread-move authority bound to its exact
 * move, case and thread. Over the reveal, timeout and thread close a funded
 * watcher built and landed, the authority's checks refuse each binding
 * changed on its own: the workflow, the policy's terms, the action the
 * reservation began, the move's thread and header and deployment; our case
 * as the reservation's lineage holds it; the spent thread's exact output,
 * validator, token, prover and case; and a close's case input.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { exactResolvedOutputCbor } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import {
  assertValidationDisputeMoveBinding,
  assertValidationDisputeMoveCase,
  assertValidationDisputeMoveCaseInput,
  assertValidationDisputeMoveThread,
  VALIDATION_DISPUTE_MOVE_BINDING,
  type ValidationDisputeMoveAuthority,
} from "../src/workflow/funding-reservation-permit.validation-dispute-move-authority.js";
import {
  caseOriginLineage,
  disputeTerms,
  fundingPolicy,
  movesOf,
  moveTerms,
  permitState,
  refusalOf,
} from "./support/validation-trace-dispute-funded-move-walk.js";
import {
  type CapturedMove,
  type FundedMoves,
  openDefendedFundedCase,
  playFundedMovesToClose,
} from "./support/validation-trace-dispute-funded-moves.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";
import { threadUnitOf } from "./support/validation-trace-dispute-scripted-defender.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let journey: FundedMoves;
let reveal: CapturedMove;
let close: CapturedMove;

const ANOTHER = "00".repeat(32);
const refused = (...names: string[]) =>
  `${VALIDATION_DISPUTE_MOVE_BINDING} (${names.join(", ")})`;

/** `unit` with its last byte changed: another token of the same policy. */
const anotherUnit = (unit: string) =>
  unit.slice(0, -2) + (unit.endsWith("00") ? "01" : "00");

const caseId = () => caseUnit().slice(56);
const caseUnit = () =>
  Object.keys(journey.caseOrigin.assets).find((unit) =>
    unit.startsWith(journey.scene.chain.case.policyId),
  )!;

/** The binding of `move`'s authority, with each input as given. */
const bind = (
  move: CapturedMove,
  {
    authority = move.authority,
    policy = fundingPolicy(journey),
    state = {},
    action = move.action,
    headerHash = move.authority.headerHash,
  }: {
    readonly authority?: ValidationDisputeMoveAuthority;
    readonly policy?: ReturnType<typeof fundingPolicy>;
    readonly state?: Record<string, unknown>;
    readonly action?: CapturedMove["action"];
    readonly headerHash?: string;
  } = {},
) =>
  assertValidationDisputeMoveBinding({
    authority,
    policy,
    state: { ...(permitState(journey, move) as object), ...state } as never,
    action,
    headerHash,
  });

/** Our case read from `lineage` under `move`'s authority. */
const readCase = (move: CapturedMove, lineage: unknown) =>
  assertValidationDisputeMoveCase({
    authority: move.authority,
    terms: disputeTerms(journey),
    lineage,
    fundingPaymentKeyHash: journey.scene.h.proverSigner.paymentKeyHash,
  });

/** The lineage of a case origin `edit` made, at its own out-ref. */
const originLineage = (edit: (origin: UTxO) => UTxO) => ({
  ...caseOriginLineage(journey),
  resolvedOutputCborHex: exactResolvedOutputCbor(edit(journey.caseOrigin)),
});

/** The case origin with its datum's `field` set to `value`. */
const originDatum = (
  field: "block_header_hash" | "bounty_claimant",
  value: string,
) =>
  originLineage((origin) => ({
    ...origin,
    datum: Data.to(
      { ...Data.from(origin.datum!, SDK.Case), [field]: value },
      SDK.Case,
    ),
  }));

/**
 * `move`'s spent thread made by `edit`, presented with an authority granted
 * over that thread's exact output (unless `authority` says otherwise).
 */
const presentThread = (
  move: CapturedMove,
  edit: (thread: UTxO) => UTxO,
  authority: Partial<ValidationDisputeMoveAuthority> = {},
) => {
  const thread = edit(move.thread);
  return assertValidationDisputeMoveThread({
    authority: {
      ...move.authority,
      threadResolvedOutputCborHex: exactResolvedOutputCbor(thread),
      ...authority,
    },
    terms: disputeTerms(journey),
    moves: moveTerms(journey),
    caseId: caseId(),
    thread,
    fundingPaymentKeyHash: journey.scene.h.proverSigner.paymentKeyHash,
  });
};

/** The thread with its datum edited by `edit`. */
const threadDatum =
  (edit: (datum: SDK.CaseThreadDatum) => SDK.CaseThreadDatum) =>
  (thread: UTxO): UTxO => ({
    ...thread,
    datum: Data.to(
      edit(Data.from(thread.datum!, SDK.CaseThreadDatum)),
      SDK.CaseThreadDatum,
    ),
  });

/** A close's case input made by `edit`, with an authority over it. */
const presentCaseInput = (
  edit: (caseInput: UTxO) => UTxO,
  authority: Partial<ValidationDisputeMoveAuthority> = {},
) => {
  const caseInput = edit(close.caseInput!);
  return assertValidationDisputeMoveCaseInput({
    authority: {
      ...close.authority,
      caseResolvedOutputCborHex: exactResolvedOutputCbor(caseInput),
      ...authority,
    },
    terms: disputeTerms(journey),
    caseId: caseId(),
    caseInput,
  });
};

describe("validation-trace dispute thread-move authority bound to its exact move", () => {
  beforeAll(async () => {
    journey = await openDefendedFundedCase();
    await playFundedMovesToClose(journey);
    reveal = movesOf(journey, "reveal")[0]!;
    close = movesOf(journey, "close_thread")[0]!;
  }, TIMEOUT);

  it("admits each move the watcher built as its builder evaluated it", async () => {
    for (const move of journey.moves) {
      expect(await refusalOf(() => bind(move))).toBeNull();
      expect(readCase(move, caseOriginLineage(journey))).toBe(caseId());
      expect(presentThread(move, (thread) => thread)).toBe(
        threadUnitOf(journey.scene, move.thread).slice(56),
      );
    }
    expect(
      await refusalOf(() => presentCaseInput((input) => input)),
    ).toBeNull();
  });

  it.each([
    {
      name: "another workflow's category",
      check: () => bind(reveal, { state: { category: "doubleSpend" } }),
      refusal: refused("category"),
    },
    {
      name: "another category's policy",
      check: () =>
        bind(reveal, {
          policy: { ...fundingPolicy(journey), category: "doubleSpend" },
        }),
      refusal: refused("policy category"),
    },
    {
      name: "a policy without thread-move terms",
      check: () =>
        bind(reveal, {
          policy: fundingPolicy(journey, {
            ...disputeTerms(journey),
            moves: undefined,
          } as never),
        }),
      refusal: refused("move terms"),
    },
    {
      name: "a reservation begun for another action kind",
      check: () => bind(reveal, { state: { currentActionKind: "timeout" } }),
      refusal: refused("current action kind"),
    },
    {
      name: "an authority for another kind of move than its action",
      check: () =>
        bind(reveal, {
          authority: { ...reveal.authority, kind: "timeout" },
          state: { currentActionKind: "timeout" },
        }),
      refusal: refused("action kind"),
    },
    {
      name: "an action other than the one the reservation began",
      check: () =>
        bind(reveal, {
          action: { ...reveal.action, actionId: `${reveal.action.actionId}'` },
        }),
      refusal: refused("action digest"),
    },
    {
      name: "an authority over another thread than its action spends",
      check: () =>
        bind(reveal, {
          authority: { ...reveal.authority, threadOutRef: `${ANOTHER}#0` },
        }),
      refusal: refused("thread"),
    },
    {
      name: "an authority for another header than the actuation's",
      check: () => bind(reveal, { headerHash: ANOTHER }),
      refusal: refused("header hash"),
    },
    {
      name: "an authority for another deployment",
      check: () =>
        bind(reveal, {
          authority: { ...reveal.authority, deploymentFingerprint: ANOTHER },
        }),
      refusal: refused("deployment fingerprint"),
    },
    {
      name: "no lineage for our case",
      check: () => readCase(reveal, null),
      refusal: refused("case origin lineage"),
    },
    {
      name: "a case lineage from another action than VerifySource",
      check: () =>
        readCase(reveal, {
          ...caseOriginLineage(journey),
          sourceActionKind: "open",
        }),
      refusal: refused("case origin lineage"),
    },
    {
      name: "the lineage of another output than the authority's case",
      check: () =>
        readCase(reveal, {
          ...caseOriginLineage(journey),
          outRef: `${ANOTHER}#0`,
        }),
      refusal: refused("case origin lineage"),
    },
    {
      name: "a case origin at another contract",
      check: () =>
        readCase(
          reveal,
          originLineage((origin) => ({
            ...origin,
            address: journey.scene.chain.game.spendingScriptAddress,
          })),
        ),
      refusal: refused("case origin address"),
    },
    {
      name: "a case origin holding a second case-policy token",
      check: () =>
        readCase(
          reveal,
          originLineage((origin) => ({
            ...origin,
            assets: { ...origin.assets, [anotherUnit(caseUnit())]: 1n },
          })),
        ),
      refusal: refused("case origin token"),
    },
    {
      name: "a case origin for another header",
      check: () =>
        readCase(
          reveal,
          originDatum("block_header_hash", ANOTHER.slice(0, 56)),
        ),
      refusal: refused("case origin header"),
    },
    {
      name: "a case origin another key claims",
      check: () =>
        readCase(reveal, originDatum("bounty_claimant", ANOTHER.slice(0, 56))),
      refusal: refused("case origin claimant"),
    },
    {
      name: "a spent thread that holds no thread datum",
      check: () =>
        presentThread(reveal, (thread) => ({ ...thread, datum: Data.void() })),
      refusal: refused("thread datum"),
    },
    {
      name: "a spent thread other than the authority's exact output",
      check: () =>
        presentThread(reveal, (thread) => thread, {
          threadResolvedOutputCborHex: exactResolvedOutputCbor({
            ...reveal.thread,
            assets: {
              ...reveal.thread.assets,
              lovelace: (reveal.thread.assets.lovelace ?? 0n) + 1n,
            },
          }),
        }),
      refusal: refused("thread output"),
    },
    {
      name: "a reveal's thread at the timeout validator",
      check: () =>
        presentThread(reveal, (thread) => ({
          ...thread,
          address: journey.scene.chain.timeout.spendingScriptAddress,
        })),
      refusal: refused("thread address"),
    },
    {
      name: "a close's thread at the game",
      check: () =>
        presentThread(close, (thread) => ({
          ...thread,
          address: journey.scene.chain.game.spendingScriptAddress,
        })),
      refusal: refused("thread address"),
    },
    {
      name: "a spent thread holding another token of the case policy",
      check: () =>
        presentThread(reveal, (thread) => {
          const [unit] = Object.keys(thread.assets).filter(
            (key) => key !== "lovelace",
          );
          const { [unit!]: _, ...rest } = thread.assets;
          return { ...thread, assets: { ...rest, [anotherUnit(unit!)]: 1n } };
        }),
      refusal: refused("thread token"),
    },
    {
      name: "a spent thread another prover holds",
      check: () =>
        presentThread(
          reveal,
          threadDatum((datum) => ({
            ...datum,
            fraud_prover: ANOTHER.slice(0, 56),
          })),
        ),
      refusal: refused("thread prover"),
    },
    {
      name: "a spent thread of another case",
      check: () =>
        presentThread(
          reveal,
          threadDatum((datum) => ({
            ...datum,
            data: { ...datum.data!, case_id: ANOTHER },
          })),
        ),
      refusal: refused("thread case"),
    },
    {
      name: "a close's case input other than the authority's exact output",
      check: () =>
        presentCaseInput((input) => input, {
          caseResolvedOutputCborHex: exactResolvedOutputCbor({
            ...close.caseInput!,
            assets: {
              ...close.caseInput!.assets,
              lovelace: (close.caseInput!.assets.lovelace ?? 0n) + 1n,
            },
          }),
        }),
      refusal: refused("case input"),
    },
    {
      name: "a close's case input at another contract",
      check: () =>
        presentCaseInput((input) => ({
          ...input,
          address: journey.scene.chain.game.spendingScriptAddress,
        })),
      refusal: refused("case input address"),
    },
    {
      name: "a close's case input without our case token",
      check: () =>
        presentCaseInput((input) => {
          const { [caseUnit()]: _, ...rest } = input.assets;
          return { ...input, assets: rest };
        }),
      refusal: refused("case input token"),
    },
  ])("refuses $name", async ({ check, refusal }) => {
    expect(await refusalOf(check)).toBe(refusal);
  });
});
