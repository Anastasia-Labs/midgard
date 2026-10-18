import { default as path } from "node:path";

import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
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

import { postSubmitHandler } from "../../src/commands/listen-router.post-submit-handler.js";
import { MempoolDB, TxAdmissionsDB } from "../../src/database/index.js";
import { AdmissionWriter } from "../../src/services/admission-writer.js";
import { NodeConfig } from "../../src/services/config.js";
import { BatchSql } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { Lucid } from "../../src/services/lucid.js";
import { MempoolLedgerCache } from "../../src/services/mempool-ledger-cache.js";
import { ContractDeploymentIdentity } from "../../src/services/midgard-contracts.js";
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
  readonly retryAfter?: string;
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
    // This fixture isolates the real durable-admission handler contract. It
    // does not claim the transport quota layer, which real socket suites cover.
    const response = yield* postSubmitHandler(
      undefined,
      wakeTxQueueProcessor,
    ).pipe(
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "derived",
          consensusProfile:
            options.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
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
        ).modify({ remoteAddress: "127.0.0.1" }),
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = yield* Effect.tryPromise({
      try: () => webResponse.json() as Promise<Record<string, unknown>>,
      catch: (cause) => cause,
    });
    const retryAfter = webResponse.headers.get("retry-after");
    return retryAfter === null
      ? { status: webResponse.status, body }
      : { status: webResponse.status, body, retryAfter };
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

export const makeScriptProofSubmitTx = (nonce: number) => {
  const scriptBytes = encodeCbor(Buffer.from("0101002499", "hex"));
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
          scriptBytes,
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
  return {
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor,
    proofEnvelope: encodeMidgardProofSubmission({
      transactionCbor: txCanonicalCbor,
    }),
  };
};

export const makeReferenceScriptProofSubmitTx = (nonce: number) => {
  const attached = makeScriptProofSubmitTx(nonce);
  const referenceOutRef = makeOutRefCbor(
    databaseTxHash(`accepted-reference-script-${nonce.toString()}`),
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

export const wrapNativeSubmitTx = (txCanonicalCbor: Buffer): Buffer =>
  encodeMidgardProofSubmission({
    transactionCbor: txCanonicalCbor,
  });

export const databaseTxHash = (label: string): Buffer =>
  deterministicFixtureTxHash(`database:${label}`);
