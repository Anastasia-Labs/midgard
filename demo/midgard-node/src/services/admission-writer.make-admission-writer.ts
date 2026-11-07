import { SqlClient } from "@effect/sql";
import { Effect, Layer } from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { makeAdmissionWriterWithOptions } from "./admission-writer.make-admission-writer-with-options.js";
import {
  AdmissionWriter,
  DEFAULT_OPTIONS,
} from "./admission-writer.validate-options.js";
import { NodeConfig } from "./config.js";
import { AdmissionSql } from "./database.js";

export const makeAdmissionWriter = Effect.gen(function* () {
  const admissionSql = yield* AdmissionSql;
  const nodeConfig = yield* NodeConfig;
  return yield* makeAdmissionWriterWithOptions(
    (requests) =>
      TxAdmissionsDB.admitReservedBatch(requests).pipe(
        Effect.provideService(SqlClient.SqlClient, admissionSql),
      ),
    {
      ...DEFAULT_OPTIONS,
      recentTxIdMaxAgeMs: nodeConfig.ADMISSION_TERMINAL_RETENTION_MS,
    },
  );
});

export const AdmissionWriterLive = Layer.scoped(
  AdmissionWriter,
  makeAdmissionWriter,
);
