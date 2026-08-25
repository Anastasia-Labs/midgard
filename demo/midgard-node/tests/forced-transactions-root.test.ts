import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { ForcedTransactionsDB } from "@/database/index.js";
import type { DatabaseError } from "@/database/utils/common.js";
import { resolveForcedTransactionsRoot } from "@/workers/commit-block-header/event-roots.js";
import {
  buildAuthenticatedRootFromEncodedEntries,
  buildRootMembershipProof,
  verifyRootMembershipProof,
} from "@/workers/commit-block-header/transition-roots.js";
import { keyValuePhasProof } from "@/workers/utils/mpf.js";

import { deterministicFixtureTxHash } from "./utils.js";

const h32 = (label: string): string =>
  deterministicFixtureTxHash(label).toString("hex");

const outputReference = (
  label: string,
  outputIndex: bigint,
): SDK.OutputReference => ({
  transactionId: h32(label),
  outputIndex,
});

const outputReferenceCbor = (value: SDK.OutputReference): Buffer =>
  Buffer.from(Data.to(value, SDK.OutputReference), "hex");

const canonicalTransaction = (): MidgardNativeTxCanonicalV1 => ({
  version: MIDGARD_NATIVE_TX_V1_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  },
});

const forcedEntry = ({
  label,
  txOrderId,
  verdict,
  inclusionTime,
}: {
  readonly label: string;
  readonly txOrderId: SDK.OutputReference;
  readonly verdict: SDK.OperatorVerdictV1;
  readonly inclusionTime: Date;
}): Effect.Effect<ForcedTransactionsDB.Entry, DatabaseError> =>
  Effect.gen(function* () {
    const nativeTxCbor = encodeMidgardNativeTxCanonicalV1(
      materializeMidgardNativeTxFromCanonicalV1(canonicalTransaction()),
    );
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    });
    const sidecarCbor = encodeMidgardCekProgramMaterialSidecarV1([]);
    return {
      [ForcedTransactionsDB.Columns.TX_ORDER_ID]:
        outputReferenceCbor(txOrderId),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]:
        deterministicFixtureTxHash(`l1-${label}`),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: Number(
        txOrderId.outputIndex,
      ),
      [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from(
        `aa${label}`,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from(
        `d879${label}`,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
      [ForcedTransactionsDB.Columns.TX_COMPACT]: encoded.txCompact,
      [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
      [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]:
        ForcedTransactionsDB.midgardTxValidityOfVerdictV1(verdict),
      [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
        MIDGARD_CONSENSUS_PROFILE_V1.profileId,
      [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: nativeTxCbor,
      [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
        encoded.transactionCommitment,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
        sidecarCbor,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
        createHash("sha256").update(sidecarCbor).digest(),
      [ForcedTransactionsDB.Columns.INCLUSION_TIME]: inclusionTime,
      [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
      [ForcedTransactionsDB.Columns.STATUS]:
        ForcedTransactionsDB.Status.Awaiting,
    };
  });

describe("forced transaction source roots", () => {
  it.effect(
    "keys forced source events by L1 tx-order output reference, not L2 tx id",
    () =>
      Effect.gen(function* () {
        const first = yield* forcedEntry({
          label: "01",
          txOrderId: outputReference("same-l2-first-order", 0n),
          verdict: "ForcedTxValid",
          inclusionTime: new Date("2026-06-12T00:00:01.000Z"),
        });
        const second = yield* forcedEntry({
          label: "02",
          txOrderId: outputReference("same-l2-second-order", 1n),
          verdict: "ForcedTxValid",
          inclusionTime: new Date("2026-06-12T00:00:02.000Z"),
        });

        expect(first[ForcedTransactionsDB.Columns.TX_ID]).toEqual(
          second[ForcedTransactionsDB.Columns.TX_ID],
        );
        expect(first[ForcedTransactionsDB.Columns.TX_ORDER_ID]).not.toEqual(
          second[ForcedTransactionsDB.Columns.TX_ORDER_ID],
        );

        const root = yield* resolveForcedTransactionsRoot([first, second]);
        expect(root._tag).toBe("Some");
        if (root._tag === "Some") {
          expect(root.value).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
          const emptyCommit = yield* SDK.commitCountedRootProgram({
            domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
            phasRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            count: 0n,
          });
          expect(root.value).not.toBe(emptyCommit);
        }
      }),
  );

  it.effect("includes invalid forced transactions as source events", () =>
    Effect.gen(function* () {
      const txOrderId = outputReference("invalid-forced-order", 0n);
      const invalid = yield* forcedEntry({
        label: "03",
        txOrderId,
        verdict: {
          ForcedTxInvalid: {
            reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
          },
        },
        inclusionTime: new Date("2026-06-12T00:00:03.000Z"),
      });
      const built = yield* buildAuthenticatedRootFromEncodedEntries(
        SDK.ROOT_DOMAINS.forcedTransactionsV1,
        [ForcedTransactionsDB.toRootKeyValue(invalid)],
      );
      const decodedValue = Data.from(
        invalid[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE].toString(
          "hex",
        ),
        SDK.ForcedInclusionTxV1,
      ) as SDK.ForcedInclusionTxV1;
      const membership = yield* buildRootMembershipProof({
        root: {
          ...built,
          typedEntries: [
            {
              key: txOrderId,
              value: decodedValue,
            },
          ],
        },
        key: txOrderId,
        value: decodedValue,
        keySchema: SDK.OutputReferenceSchema,
        valueSchema: SDK.ForcedInclusionTxV1Schema,
      });

      expect(decodedValue.verdict).toEqual({
        ForcedTxInvalid: {
          reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
        },
      });
      expect(invalid[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]).toBe(
        "TxIsInvalid",
      );
      expect(decodedValue.tx_id).toBe(
        invalid[ForcedTransactionsDB.Columns.TX_ID].toString("hex"),
      );
      yield* verifyRootMembershipProof({
        witness: membership,
        keySchema: SDK.OutputReferenceSchema,
        valueSchema: SDK.ForcedInclusionTxV1Schema,
        options: {
          expectedDomain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
          expectedRoot: built.root,
          expectedCount: 1n,
        },
      });
    }),
  );

  it.effect("rejects duplicate tx-order source keys", () =>
    Effect.gen(function* () {
      const txOrderId = outputReference("duplicate-forced-order", 0n);
      const first = yield* forcedEntry({
        label: "04",
        txOrderId,
        verdict: "ForcedTxValid",
        inclusionTime: new Date("2026-06-12T00:00:04.000Z"),
      });
      const second = {
        ...first,
        [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: Buffer.from(
          first[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
        ),
      };
      second[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE][0] ^= 1;

      const result = yield* resolveForcedTransactionsRoot([first, second]).pipe(
        Effect.either,
      );

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect(
    "domain-separates forced tx-order settlement proofs from transactions_root",
    () =>
      Effect.gen(function* () {
        const txOrderId = outputReference("settlement-forced-order", 0n);
        const forced = yield* forcedEntry({
          label: "05",
          txOrderId,
          verdict: "ForcedTxValid",
          inclusionTime: new Date("2026-06-12T00:00:05.000Z"),
        });
        const entry = ForcedTransactionsDB.toRootKeyValue(forced);
        const forcedRoot = yield* buildAuthenticatedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
          [entry],
        );
        const transactionsRoot =
          yield* buildAuthenticatedRootFromEncodedEntries(
            SDK.ROOT_DOMAINS.transactionsV1,
            [entry],
          );
        const proof = yield* keyValuePhasProof(
          [entry.key],
          [entry.value],
          entry.key,
        );
        const txOrderSettlementProof: SDK.RawRootMembershipProof = {
          domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
          root: forcedRoot.root,
          phas_root: forcedRoot.phasRoot,
          count: forcedRoot.count,
          key: entry.key.toString("hex"),
          value: entry.value.toString("hex"),
          proof,
        };

        expect(forcedRoot.phasRoot).toBe(transactionsRoot.phasRoot);
        expect(txOrderSettlementProof.root).toBe(forcedRoot.root);
        expect(txOrderSettlementProof.root).not.toBe(transactionsRoot.root);
        expect(txOrderSettlementProof.domain).toBe(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
        );
      }),
  );
});
