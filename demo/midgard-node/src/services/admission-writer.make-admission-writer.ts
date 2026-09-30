import { SqlClient } from "@effect/sql";
import { Effect, Layer } from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { makeAdmissionWriterWithOptions } from "./admission-writer.make-admission-writer-with-options.js";
import { AdmissionWriter } from "./admission-writer.validate-options.js";
import { AdmissionSql } from "./database.js";

export const makeAdmissionWriter = Effect.gen(function* () {
  const admissionSql = yield* AdmissionSql;
  return yield* makeAdmissionWriterWithOptions((requests) =>
    TxAdmissionsDB.admitReservedBatch(requests).pipe(
      Effect.provideService(SqlClient.SqlClient, admissionSql),
    ),
  );
});

export const AdmissionWriterLive = Layer.scoped(
  AdmissionWriter,
  makeAdmissionWriter,
);
