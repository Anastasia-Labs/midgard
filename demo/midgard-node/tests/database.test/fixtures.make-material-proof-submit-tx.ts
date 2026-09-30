import { createHash } from "node:crypto";
import { default as path } from "node:path";

import {
  decodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  encodeMidgardProofSubmission,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect } from "vitest";

import { buildSubmitRouter } from "../../src/commands/listen-router.js";
import {
  CekProgramMaterialDB,
  MempoolDB,
  TxAdmissionsDB,
} from "../../src/database/index.js";
import { AdmissionWriter } from "../../src/services/admission-writer.js";
import { NodeConfig } from "../../src/services/config.js";
import { BatchSql } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { Lucid } from "../../src/services/lucid.js";
import { MempoolLedgerCache } from "../../src/services/mempool-ledger-cache.js";
import { ValidationPool } from "../../src/services/validation-pool.js";
import { WriteBehind } from "../../src/services/write-behind.js";
import { makeCardanoSignedMapOutputTxBytes } from ".././helpers/cardano-native-fixtures.js";
import { makeOutRefCbor } from ".././midgard-output-helpers.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureTxHash,
  provideDatabaseLayers,
  resetApplicationTables,
} from ".././utils.js";

export const databaseTestDirectory = path.resolve(__dirname, "..");

export const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );

type SubmitHttpResult = {
  readonly status: number;
  readonly body: Record<string, unknown>;
};

export type TxQueueWakeRequirements =
  | BatchSql
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache;

export const submitThroughRouter = <R>(
  requestBody: Buffer,
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
  options: {
    readonly contentType?: string;
    readonly consensusProfile?: MidgardConsensusProfile;
  } = {},
): Effect.Effect<
  SubmitHttpResult,
  unknown,
  R | SqlClient.SqlClient | NodeConfig | Globals | AdmissionWriter
> =>
  Effect.gen(function* () {
    const response = yield* buildSubmitRouter(
      wakeTxQueueProcessor,
      undefined,
      options.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE,
    ).pipe(
      Effect.provideService(
        HttpServerRequest.HttpServerRequest,
        HttpServerRequest.fromWeb(
          new Request("http://midgard.test/submit", {
            method: "POST",
            headers: {
              "content-type":
                options.contentType ?? "application/vnd.midgard.v1+cbor",
              "content-length": requestBody.length.toString(),
            },
            body: new Uint8Array(requestBody),
          }),
        ),
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = yield* Effect.tryPromise({
      try: () => webResponse.json() as Promise<Record<string, unknown>>,
      catch: (cause) => cause,
    });
    return { status: webResponse.status, body };
  });

export const makeNativeSubmitTx = (): {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
} => {
  const txCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
    makeCardanoSignedMapOutputTxBytes(),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return { txId, txIdHex: txId.toString("hex"), txCanonicalCbor };
};

export const makeProofSubmitTx = (): {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
} => {
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
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
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return { txId, txIdHex: txId.toString("hex"), txCanonicalCbor };
};

export const makeMaterialProofSubmitTx = (nonce: number) => {
  const term = { kind: "variable" as const, index: BigInt(nonce) };
  const preimage = encodeMidgardCekTermNode(term);
  const material = {
    kind: "term" as const,
    root: hashMidgardCekTermNode(term),
    preimage,
  };
  const envelope = decodeMidgardCekProgramEnvelope(
    encodeMidgardCekProgramEnvelope({
      uplcVersion: [1n, 1n, 0n],
      termRoot: material.root,
      nodeCount: 1n,
      materialByteLength: BigInt(preimage.length),
    }),
  );
  const base = decodeMidgardNativeTxFullFromCanonicalCbor(
    makeNativeSubmitTx().txCanonicalCbor,
  );
  const canonical: MidgardNativeTxCanonical = {
    version: base.version,
    validity: base.validity,
    body: {
      ...base.body,
      fee: base.body.fee + BigInt(nonce),
    },
    witnessSet: {
      ...base.witnessSet,
      scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
        {
          language: "MidgardV1",
          scriptBytes: encodeMidgardCekProgramEnvelope(envelope),
        },
      ]),
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  const sidecarCbor = encodeMidgardCekProgramMaterialSidecar([material]);
  return {
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor,
    material,
    envelope,
    sidecarCbor,
    proofEnvelope: encodeMidgardProofSubmission({
      transactionCbor: txCanonicalCbor,
      programMaterial: [material],
    }),
  };
};

export const makeReferenceMaterialProofSubmitTx = (nonce: number) => {
  const attached = makeMaterialProofSubmitTx(nonce);
  const referenceOutRef = makeOutRefCbor(
    databaseTxHash(`accepted-reference-material-${nonce.toString()}`),
    0,
  );
  const attachedCanonical = decodeMidgardNativeTxFullFromCanonicalCbor(
    attached.txCanonicalCbor,
  );
  const canonical: MidgardNativeTxCanonical = {
    ...attachedCanonical,
    body: {
      ...attachedCanonical.body,
      referenceInputsPreimageCbor: encodeCbor([referenceOutRef]),
    },
    witnessSet: {
      ...attachedCanonical.witnessSet,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return {
    ...attached,
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor,
    referenceOutRef,
    proofEnvelope: encodeMidgardProofSubmission({
      transactionCbor: txCanonicalCbor,
      programMaterial: [attached.material],
    }),
  };
};

export const expectSubmitBody = (
  result: SubmitHttpResult,
  expected: {
    readonly status: 200 | 202;
    readonly txIdHex: string;
    readonly duplicate: boolean;
  },
): void => {
  expect(result.status).toBe(expected.status);
  expect(result.body).toEqual({
    txId: expected.txIdHex,
    status: TxAdmissionsDB.Status.Queued,
    firstSeenAt: expect.any(String),
    lastSeenAt: expect.any(String),
    duplicate: expected.duplicate,
  });
};

export const retrieveAllMempool = MempoolDB.retrievePage({
  limit: 100_000,
}).pipe(Effect.map((page) => page.entries));

export const databaseFixtureBytes = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`database:${label}`, length);

export const emptyProgramMaterialSidecar =
  encodeMidgardCekProgramMaterialSidecar([]);

export const emptyProgramMaterialSidecarSha256 = createHash("sha256")
  .update(emptyProgramMaterialSidecar)
  .digest();

export const readCekProgramMaterialStoreStats = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly entry_count: string;
    readonly membership_count: string;
    readonly owner_count: string;
    readonly total_bytes: string;
  }>`SELECT
      (SELECT COUNT(*)::text
        FROM ${sql(CekProgramMaterialDB.entryTableName)}) AS entry_count,
      (SELECT COUNT(*)::text
        FROM ${sql(CekProgramMaterialDB.membershipTableName)})
        AS membership_count,
      (SELECT COUNT(*)::text
        FROM ${sql(CekProgramMaterialDB.admissionOwnerTableName)})
        AS owner_count,
      (
        COALESCE((
          SELECT SUM(
            octet_length(material_root) + octet_length(da_value_cbor)
          )
          FROM ${sql(CekProgramMaterialDB.entryTableName)}
        ), 0)
        + COALESCE((
          SELECT SUM(
            octet_length(program_envelope_hash)
              + octet_length(material_root)
              + 1
          )
          FROM ${sql(CekProgramMaterialDB.membershipTableName)}
        ), 0)
        + COALESCE((
          SELECT SUM(
            octet_length(tx_id)
              + octet_length(program_envelope_hash)
              + octet_length(material_root)
          )
          FROM ${sql(CekProgramMaterialDB.admissionOwnerTableName)}
        ), 0)
      )::text AS total_bytes`;
  return rows[0]!;
});

export const wrapNativeSubmitTx = (txCanonicalCbor: Buffer): Buffer =>
  encodeMidgardProofSubmission({
    transactionCbor: txCanonicalCbor,
    programMaterial: [],
  });

export const databaseTxHash = (label: string): Buffer =>
  deterministicFixtureTxHash(`database:${label}`);
