/**
 * The **row definitions** behind the ledger output proof replay golden
 * vectors — one source of truth, driven from two sides.
 *
 * The generator (`scripts/generate-ledger-output-proof-replay-goldens.mjs`)
 * drives these rows through the built `dist/` twins to emit the JSON fixture
 * and rebind the named constants of
 * `onchain/aiken/lib/midgard/ledger-output-proof-datum.test.ak`; the vitest
 * suites (`tests/ledger-output-proof-empty-span.test.ts` and
 * `tests/ledger-output-proof-split-span.test.ts`) drive the same rows through
 * `src/` and check the result against the checked-in fixture. The Aiken tests
 * replay each row's witness list through the typed and split on-chain steps to
 * the same terminal control.
 *
 * A row is an output (a datum, and optionally a reference script or an asset)
 * plus the claims that make it worth having: its step count, how many datum
 * steps read an empty span and, for the datum-scalar rows, the shape of each
 * datum-stage span attach. The claims are written here by hand; the generator
 * refuses to emit a row whose producer no longer exhibits them.
 *
 * This module imports nothing from the package, so it can be loaded by either
 * side without deciding which build of the trace builder is under test: the
 * package functions are passed in as `twins`.
 */

import { Constr } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

/** The address every row's output pays to. */
const ADDRESS_HEX = `78${"11".repeat(28)}`;

/** An indefinite byte string of `length` bytes `fill` in 64-byte chunks. */
const chunkedBytes = (length, fill) => {
  const parts = ["5f"];
  for (let at = 0; at < length; at += 64) {
    const size = Math.min(64, length - at);
    const head =
      size < 24 ? (0x40 + size).toString(16) : `58${size.toString(16)}`;
    parts.push(head, fill.repeat(size));
  }
  parts.push("ff");
  return parts.join("");
};

/**
 * Outputs that hold an empty field and fit one chunk. Their chunk and
 * span-attach witnesses are written as bare constructors (`constr 1 []`,
 * `constr 5 []`) and rebuilt on-chain from the single chunk.
 */
export const EMPTY_FIELD_ROWS = [
  {
    id: "empty_bytes_last",
    label: "an empty byte string datum ending the output",
    datumHex: "40",
    steps: 30,
    emptySteps: 1,
  },
  {
    id: "list_holding_empty_bytes",
    label: "a list holding an empty byte string",
    datumHex: "9f40ff",
    steps: 34,
    emptySteps: 1,
  },
  {
    id: "list_starting_with_empty_bytes",
    label: "a list starting with an empty byte string",
    datumHex: "9f4001ff",
    steps: 54,
    emptySteps: 1,
  },
  {
    id: "constr_holding_empty_bytes",
    label: "a constructor holding an empty byte string",
    datumHex: "d8799f40ff",
    steps: 34,
    emptySteps: 1,
  },
  {
    id: "constr_starting_with_empty_bytes",
    label: "a constructor starting with an empty byte string",
    datumHex: "d8799f4001ff",
    steps: 54,
    emptySteps: 1,
  },
  {
    id: "map_with_empty_bytes_key",
    label: "a map with an empty byte string key",
    datumHex: "a14000",
    steps: 52,
    emptySteps: 1,
  },
  {
    id: "map_with_empty_bytes_value",
    label: "a map with an empty byte string value",
    datumHex: "a10040",
    steps: 52,
    emptySteps: 1,
  },
  {
    id: "empty_bytes_before_a_script",
    label: "an empty byte string datum before a reference script",
    datumHex: "40",
    scriptHex: "6b".repeat(10),
    steps: 49,
    emptySteps: 1,
  },
  {
    id: "empty_asset_name_and_bytes",
    label: "an empty asset name and an empty byte string datum",
    datumHex: "40",
    asset: { policyHex: "55".repeat(28), nameHex: "", quantity: 1n },
    steps: 33,
    emptySteps: 1,
  },
  {
    id: "empty_reference_script",
    label: "an empty reference script",
    datumHex: "00",
    scriptHex: "",
    steps: 47,
    emptySteps: 0,
  },
  {
    id: "one_byte_string",
    label: "a one-byte byte string datum",
    datumHex: "4101",
    steps: 30,
    emptySteps: 0,
  },
];

/**
 * Outputs whose datum holds a large chunked scalar, so span attaches occur
 * at the bytes and integer stages of the datum traversal. Their chunk and
 * span-attach witnesses are written as their constructor over the chunk
 * indices `[first, next or -1]`, and a datum witness whose window is the
 * recorded window as `constr 9` over its action alone; the Aiken tests
 * rebuild both from the output's chunk proofs and recorded window.
 *
 * `attaches` lists each datum-stage span attach as `100 * traverse stage +
 * 10 * (blob present) + (blob hash present)`.
 */
export const DATUM_SCALAR_ROWS = [
  {
    id: "large_bytes_datum",
    label: "a 5000-byte byte string datum",
    datumHex: chunkedBytes(5_000, "ab"),
    total: 5_204,
    steps: 674,
    emptySteps: 0,
    attaches: [0, 200, 211, 211],
  },
  {
    id: "large_integer_datum",
    label: "an integer datum over a 5000-byte byte string",
    datumHex: `c2${chunkedBytes(5_000, "01")}`,
    total: 5_205,
    steps: 688,
    emptySteps: 0,
    attaches: [0, 100, 111, 111],
  },
  {
    id: "empty_bytes_ending_the_first_chunk",
    label: "an empty byte string ending the first chunk",
    datumHex: `9f${chunkedBytes(3_922, "ab")}40ff`,
    total: 4_095,
    steps: 536,
    emptySteps: 1,
    attaches: [0],
  },
  {
    id: "empty_bytes_starting_the_second_chunk",
    label: "an empty byte string starting the second chunk",
    datumHex: `9f${chunkedBytes(3_923, "ab")}40ff`,
    total: 4_096,
    steps: 537,
    emptySteps: 1,
    attaches: [0],
  },
  {
    id: "empty_bytes_inside_the_second_chunk",
    label: "an empty byte string inside the second chunk",
    datumHex: `9f${chunkedBytes(3_924, "ab")}40ff`,
    total: 4_097,
    steps: 537,
    emptySteps: 1,
    attaches: [0],
  },
];

const hash = (bytes) => Buffer.from(blake2b(bytes, { dkLen: 32 }));

const rowOutput = (row, twins) =>
  Buffer.from(
    twins.encodeMidgardTxOutput({
      address: Buffer.from(ADDRESS_HEX, "hex"),
      value: {
        lovelace: 2_000_000n,
        assets:
          row.asset === undefined
            ? new Map()
            : new Map([
                [
                  row.asset.policyHex,
                  new Map([[row.asset.nameHex, row.asset.quantity]]),
                ],
              ]),
      },
      datum: twins.decodeMidgardDatum(Buffer.from(row.datumHex, "hex")),
      ...(row.scriptHex === undefined
        ? {}
        : {
            script_ref: {
              language: "PlutusV3",
              scriptBytes: Buffer.from(row.scriptHex, "hex"),
            },
          }),
    }),
  );

const isChunkWitness = (witness) =>
  witness !== null &&
  (witness.kind === "chunks" || witness.kind === "spanAttach");

const chunkConstructor = (witness) => (witness.kind === "chunks" ? 1 : 5);

/** A one-chunk output's witness: chunk witnesses as bare constructors. */
const emptyFieldWitness = (twins, { witness }) => {
  if (isChunkWitness(witness)) {
    if (
      witness.chunkProof.chunkIndex !== 0 ||
      witness.nextChunkProof !== null
    ) {
      throw new Error("an empty-field row must fit one chunk");
    }
    return new Constr(chunkConstructor(witness), []);
  }
  return twins.witnessData(witness);
};

/**
 * A chunked output's witness: chunk witnesses over their chunk indices, and a
 * datum witness over its action alone when its window is the recorded one.
 */
const datumScalarWitness = (twins, output, { control, witness }) => {
  if (isChunkWitness(witness)) {
    return new Constr(chunkConstructor(witness), [
      BigInt(witness.chunkProof.chunkIndex),
      witness.nextChunkProof === null
        ? -1n
        : BigInt(witness.nextChunkProof.chunkIndex),
    ]);
  }
  if (witness?.kind === "datum" && witness.window !== null) {
    const recorded = control.spanWindow;
    const window = output.subarray(
      recorded.start,
      recorded.start + recorded.length,
    );
    if (!Buffer.from(witness.window).equals(window)) {
      throw new Error("a datum window is not the recorded window");
    }
    return new Constr(9, [twins.witnessData(witness).fields[0]]);
  }
  return twins.witnessData(witness);
};

/**
 * Each datum-stage span attach as `100 * traverse stage + 10 * (blob present)
 * + (blob hash present)`.
 */
const attachShapes = (twins, trace) =>
  trace.steps
    .filter(
      ({ control, witness }) =>
        witness?.kind === "spanAttach" &&
        control.stage === twins.MidgardLedgerOutputProofStages.DatumTraversal,
    )
    .map(({ control }) => {
      const blob =
        control.datum.integer?.blob ?? control.datum.bytes?.blob ?? null;
      const blobShape = blob === null ? 0 : blob.activeHash === null ? 10 : 11;
      return 100 * control.datum.stage + blobShape;
    });

const emptySpanSteps = (twins, trace) =>
  trace.steps.filter(
    ({ control }) =>
      control.stage === twins.MidgardLedgerOutputProofStages.DatumTraversal &&
      control.datum !== null &&
      twins.nextMidgardCekDataTraverseSpan(control.datum)?.length === 0,
  ).length;

/**
 * What the twins produce for `row` of `family` (`"emptyField"` or
 * `"datumScalar"`): the output, its proof's witness list and terminal control,
 * and the measured claims.
 */
export const ledgerOutputProofReplayVector = (row, family, twins) => {
  const output = rowOutput(row, twins);
  const trace = twins.buildMidgardLedgerOutputProofTrace({
    outputIndex: 0,
    outputCbor: output,
  });
  const witnesses = trace.steps.map((step) =>
    family === "emptyField"
      ? emptyFieldWitness(twins, step)
      : datumScalarWitness(twins, output, step),
  );
  const witnessesCbor = Buffer.from(
    twins.plutusDataToCborHex(witnesses),
    "hex",
  );
  const terminal = Buffer.from(
    twins.encodeMidgardLedgerOutputProofControl(trace.terminal),
  );
  return {
    id: row.id,
    outputCborHex: output.toString("hex"),
    outputLength: output.length,
    steps: trace.steps.length,
    emptySteps: emptySpanSteps(twins, trace),
    attaches: attachShapes(twins, trace),
    exactTerminal: twins.isExactMidgardLedgerOutputProofTerminal(
      trace.terminal,
    ),
    witnessesCborHex: witnessesCbor.toString("hex"),
    witnessesHash: hash(witnessesCbor).toString("hex"),
    terminalHash: hash(terminal).toString("hex"),
  };
};

/** Every claim of `row` that `vector` does not exhibit, as a sentence. */
export const ledgerOutputProofReplayClaimProblems = (row, vector) => {
  const problems = [];
  const claim = (name, expected, actual) => {
    if (JSON.stringify(expected) !== JSON.stringify(actual)) {
      problems.push(
        `${row.id}: ${name} is ${JSON.stringify(actual)}, not ${JSON.stringify(expected)}`,
      );
    }
  };
  if (row.total !== undefined)
    claim("output length", row.total, vector.outputLength);
  claim("step count", row.steps, vector.steps);
  claim("empty-span step count", row.emptySteps, vector.emptySteps);
  if (row.attaches !== undefined)
    claim("attach shapes", row.attaches, vector.attaches);
  claim("exact terminal", true, vector.exactTerminal);
  return problems;
};

/** The rows of both families, each with its family. */
export const LEDGER_OUTPUT_PROOF_REPLAY_ROWS = [
  ...EMPTY_FIELD_ROWS.map((row) => ({ row, family: "emptyField" })),
  ...DATUM_SCALAR_ROWS.map((row) => ({ row, family: "datumScalar" })),
];
