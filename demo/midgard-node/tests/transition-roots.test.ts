import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import {
  buildAdjacentTraceProof,
  buildAuthenticatedRootFromDataEntries,
  buildEventToStepMembershipProof,
  buildEventToStepNonMembershipProof,
  buildEventToStepRoot,
  buildIndexedTraceProof,
  buildRootMembershipProof,
  buildRootNonMembershipProof,
  buildTransitionTraceRoot,
  verifyAdjacentTraceProof,
  verifyEventToStepProof,
  verifyIndexedTraceProof,
  verifyRootCountProof,
  verifyRootMembershipProof,
  verifyRootNonMembershipProof,
} from "../src/workers/commit-block-header/transition-roots.js";
import {
  depositEventKey,
  h28,
  h32,
  outputReference,
  withdrawalEventKey,
} from "./helpers/transition-fixtures.js";

const transitionStep = ({
  stepIndex,
  eventKey,
  pre,
  post,
  phase = "Withdrawal",
}: {
  readonly stepIndex: bigint;
  readonly eventKey: SDK.EventKey;
  readonly pre: string;
  readonly post: string;
  readonly phase?: SDK.TransitionPhase;
}): SDK.TransitionStep => ({
  schema_version: 1n,
  step_index: stepIndex,
  event_key: eventKey,
  phase,
  pre_utxos_root: pre,
  post_utxos_root: post,
});

const expectRoundTrip = <A>(value: A, schema: unknown): A =>
  Data.from(Data.to(value as never, schema as never), schema as never) as A;

describe("transition root primitives", () => {
  it.effect(
    "builds and verifies source-root membership/non-membership proofs",
    () =>
      Effect.gen(function* () {
        const root = yield* buildAuthenticatedRootFromDataEntries({
          domain: SDK.ROOT_DOMAINS.transactionsV1,
          entries: [
            { key: h32(1), value: h32(11) },
            { key: h32(2), value: h32(12) },
          ],
          keySchema: SDK.H32Schema,
          valueSchema: SDK.H32Schema,
        });
        const membership = yield* buildRootMembershipProof({
          root,
          key: h32(1),
          value: h32(11),
          keySchema: SDK.H32Schema,
          valueSchema: SDK.H32Schema,
        });
        const nonMembership = yield* buildRootNonMembershipProof({
          root,
          key: h32(3),
          keySchema: SDK.H32Schema,
        });

        yield* verifyRootMembershipProof({
          witness: membership,
          keySchema: SDK.H32Schema,
          valueSchema: SDK.H32Schema,
          options: {
            expectedDomain: SDK.ROOT_DOMAINS.transactionsV1,
            expectedRoot: root.root,
            expectedCount: 2n,
          },
        });
        yield* verifyRootNonMembershipProof({
          witness: nonMembership,
          keySchema: SDK.H32Schema,
          options: {
            expectedDomain: SDK.ROOT_DOMAINS.transactionsV1,
            expectedRoot: root.root,
            expectedCount: 2n,
          },
        });

        expectRoundTrip(
          membership,
          SDK.rootMembershipProofSchema(SDK.H32Schema, SDK.H32Schema),
        );
        expectRoundTrip(
          nonMembership,
          SDK.rootNonMembershipProofSchema(SDK.H32Schema),
        );
      }),
  );

  it.effect("rejects duplicate source-root keys", () =>
    Effect.gen(function* () {
      const result = yield* buildAuthenticatedRootFromDataEntries({
        domain: SDK.ROOT_DOMAINS.withdrawals,
        entries: [
          { key: outputReference(1), value: h28(2) },
          { key: outputReference(1), value: h28(3) },
        ],
        keySchema: SDK.OutputReferenceSchema,
        valueSchema: SDK.PubKeyHashSchema,
      }).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect(
    "builds dense transition trace proofs and rejects count mismatches",
    () =>
      Effect.gen(function* () {
        const steps = [
          transitionStep({
            stepIndex: 1n,
            eventKey: depositEventKey(2),
            phase: "Deposit",
            pre: h32(5),
            post: h32(6),
          }),
          transitionStep({
            stepIndex: 0n,
            eventKey: withdrawalEventKey(1),
            pre: h32(4),
            post: h32(5),
          }),
        ];
        const root = yield* buildTransitionTraceRoot(steps);
        const indexed = yield* buildIndexedTraceProof({
          root,
          stepIndex: 0n,
        });
        const adjacent = yield* buildAdjacentTraceProof({
          root,
          lowerStepIndex: 0n,
        });

        yield* verifyIndexedTraceProof(indexed, {
          expectedRoot: root.root,
          expectedCount: 2n,
        });
        yield* verifyAdjacentTraceProof(adjacent, {
          expectedRoot: root.root,
          expectedCount: 2n,
        });

        const wrongCount = yield* verifyIndexedTraceProof(indexed, {
          expectedRoot: root.root,
          expectedCount: 3n,
        }).pipe(Effect.either);

        expect(root.count).toBe(2n);
        expect(root.root).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
        expect(wrongCount._tag).toBe("Left");
        expectRoundTrip(indexed, SDK.IndexedTraceProof);
        expectRoundTrip(adjacent, SDK.AdjacentTraceProof);
      }),
  );

  it.effect("rejects sparse or duplicate dense transition trace indexes", () =>
    Effect.gen(function* () {
      const sparse = yield* buildTransitionTraceRoot([
        transitionStep({
          stepIndex: 1n,
          eventKey: withdrawalEventKey(1),
          pre: h32(1),
          post: h32(2),
        }),
      ]).pipe(Effect.either);
      const duplicate = yield* buildTransitionTraceRoot([
        transitionStep({
          stepIndex: 0n,
          eventKey: withdrawalEventKey(1),
          pre: h32(1),
          post: h32(2),
        }),
        transitionStep({
          stepIndex: 0n,
          eventKey: withdrawalEventKey(2),
          pre: h32(2),
          post: h32(3),
        }),
      ]).pipe(Effect.either);

      expect(sparse._tag).toBe("Left");
      expect(duplicate._tag).toBe("Left");
    }),
  );

  it.effect("proves dense trace out-of-range non-membership", () =>
    Effect.gen(function* () {
      const root = yield* buildTransitionTraceRoot([
        transitionStep({
          stepIndex: 0n,
          eventKey: withdrawalEventKey(1),
          pre: h32(1),
          post: h32(2),
        }),
      ]);
      const nonMembership = yield* buildRootNonMembershipProof({
        root,
        key: 1n,
        keySchema: Data.Integer(),
      });

      yield* verifyRootNonMembershipProof({
        witness: nonMembership,
        keySchema: Data.Integer(),
        options: {
          expectedDomain: SDK.ROOT_DOMAINS.transitionTrace,
          expectedRoot: root.root,
          expectedCount: 1n,
        },
      });
    }),
  );

  it.effect(
    "builds and verifies event-to-step membership and non-membership",
    () =>
      Effect.gen(function* () {
        const eventKey = withdrawalEventKey(1);
        const root = yield* buildEventToStepRoot([
          {
            key: eventKey,
            value: { step_index: 1n, phase: "Withdrawal" },
          },
        ]);
        const membership = yield* buildEventToStepMembershipProof({
          root,
          eventKey,
          value: { step_index: 1n, phase: "Withdrawal" },
        });
        const nonMembership = yield* buildEventToStepNonMembershipProof({
          root,
          eventKey: depositEventKey(2),
        });

        yield* verifyEventToStepProof(membership, {
          expectedRoot: root.root,
          expectedCount: 1n,
        });
        yield* verifyEventToStepProof(nonMembership, {
          expectedRoot: root.root,
          expectedCount: 1n,
        });

        if ("EventToStepMembership" in membership) {
          expect(
            membership.EventToStepMembership.membership.value.step_index,
          ).not.toBe(0n);
        }
        expectRoundTrip(membership, SDK.EventToStepProof);
        expectRoundTrip(nonMembership, SDK.EventToStepProof);
      }),
  );

  it.effect("rejects malformed root count proofs", () =>
    Effect.gen(function* () {
      const result = yield* verifyRootCountProof(
        {
          domain: SDK.ROOT_DOMAINS.eventToStep,
          root: SDK.EMPTY_MERKLE_TREE_ROOT,
          phas_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          count: 1n,
        },
        {
          expectedDomain: SDK.ROOT_DOMAINS.eventToStep,
          expectedRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          expectedCount: 1n,
        },
      ).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );
});
