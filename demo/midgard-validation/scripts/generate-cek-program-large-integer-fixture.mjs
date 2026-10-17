/**
 * Generates `tests/fixtures/cek-program-large-integer.json`, the externally
 * supplied program material that `tests/cek-program-material-execution.test.ts`
 * decodes and executes.
 *
 * The program is `(lam ctx [(lam x (con unit ())) [(builtin addInteger)
 * (con integer 2^512) (con integer 1)]])`: it adds a 65-byte integer, whose
 * canonical CBOR is a chunked bignum, and halts with Unit. The material is
 * what the canonical program builder commits for that Flat program, written
 * as hex so the test feeds it through the material decoder rather than the
 * builder.
 *
 * Usage: node scripts/generate-cek-program-large-integer-fixture.mjs [--check]
 * (run `pnpm run build` first; it reads `dist`).
 */
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core";
import {
  goldenChannelEmitter,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import {
  Application,
  Builtin,
  Lambda,
  UPLCConst,
  UPLCProgram,
} from "@harmoniclabs/uplc";

import {
  buildMidgardCanonicalCekProgram,
  encodeMidgardCekCardanoFlatProgram,
} from "../dist/index.js";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-program-large-integer-fixture.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const flat = encodeMidgardCekCardanoFlatProgram(
  new UPLCProgram(
    [1, 1, 0],
    new Lambda(
      new Application(
        new Lambda(UPLCConst.unit),
        new Application(
          new Application(Builtin.addInteger, UPLCConst.int(1n << 512n)),
          UPLCConst.int(1),
        ),
      ),
    ),
  ),
);
const program = buildMidgardCanonicalCekProgram(flat);

const fixture = {
  envelopeCbor: Buffer.from(
    encodeMidgardCekProgramEnvelope(program.envelope),
  ).toString("hex"),
  material: [...program.material.values()].map((entry) =>
    Buffer.from(encodeMidgardCekProgramMaterialEntry(entry)).toString("hex"),
  ),
};

writeOrCheck(
  resolve(packageRoot, "tests/fixtures/cek-program-large-integer.json"),
  `${JSON.stringify(fixture, null, 2)}\n`,
);
