/** Failing PlutusV3 scripts, `(program 1.1.0 (lam ctx [(lam _ (error))
 * STEP]))` in Flat, whose run ends in a step of a staged semantics group of
 * the CEK core: the staged product loop (9), the serialise loop (10), the
 * staged conversion (11) and the writeBits loop (12). Large values are built
 * at run time, so each script stays well within the script field. */
import { encodeMidgardCekCardanoFlatProgram } from "@al-ft/midgard-validation";

/** A term as the Flat encoder reads it. */
type FlatTerm = Parameters<
  typeof encodeMidgardCekCardanoFlatProgram
>[0]["body"];

const v = (deBruijn: number): FlatTerm => ({ tag: 0, deBruijn });
const lam = (body: FlatTerm): FlatTerm => ({ tag: 2, body });
const app = (func: FlatTerm, ...args: readonly FlatTerm[]): FlatTerm =>
  args.reduce<FlatTerm>(
    (applied, arg) => ({ tag: 3, func: applied, arg }),
    func,
  );
const force = (forced: FlatTerm): FlatTerm => ({ tag: 5, forced });
const builtin = (builtinTag: number): FlatTerm => ({ tag: 7, builtinTag });
const int = (value: bigint): FlatTerm => ({ tag: 4, type: [0], value });
const bytes = (value: Uint8Array): FlatTerm => ({ tag: 4, type: [1], value });
const bool = (value: boolean): FlatTerm => ({ tag: 4, type: [4], value });
const ints = (value: readonly bigint[]): FlatTerm => ({
  tag: 4,
  type: [5, 0],
  value,
});

const MULTIPLY = 2;
const MOD = 6;
const SUBTRACT = 1;
const ADD = 0;
const APPEND = 10;
const IF_THEN_ELSE = 26;
const MK_CONS = 32;
const EQUALS_INTEGER = 7;
const B_DATA = 41;
const LIST_DATA = 39;
const SERIALISE = 51;
const BYTES_TO_INTEGER = 74;
const WRITE_BITS = 80;

const call = (tag: number, ...args: readonly FlatTerm[]): FlatTerm =>
  app(builtin(tag), ...args);

/** The failing script whose run evaluates `step`, as Flat hex. */
const failing = (step: FlatTerm): string =>
  encodeMidgardCekCardanoFlatProgram({
    version: { major: 1, minor: 1, patch: 0 },
    body: lam(app(lam({ tag: 6 }), step)),
  }).toString("hex");

/** An integer of `size` magnitude bytes. */
const wide = (size: number, low = 0n): bigint =>
  (1n << BigInt(8 * size - 1)) + low;

/** `seed` doubled `times` times by appendByteString, at run time. */
const doubled = (seed: FlatTerm, times: number): FlatTerm =>
  Array.from({ length: times }).reduce<FlatTerm>(
    (built) => app(lam(call(APPEND, v(0), v(0))), built),
    seed,
  );

const PATTERN = Uint8Array.from({ length: 64 }, (_, i) => (i * 7 + 1) & 0xff);

/** `length` bytes of the 64-byte pattern `(i * 7 + 1) mod 256` repeated,
 * built at run time from the pattern by doublings and appends. */
const builtBytes = (length: number): FlatTerm => {
  const whole = Math.floor(length / 64);
  const parts: FlatTerm[] = [];
  for (let bit = 0; 1 << bit <= whole; bit += 1)
    if (whole & (1 << bit)) parts.push(doubled(bytes(PATTERN), bit));
  if (length % 64 !== 0) parts.push(bytes(PATTERN.subarray(0, length % 64)));
  return parts.reduce((left, right) => call(APPEND, left, right));
};

/** STEP: `[(builtin multiplyInteger) [(builtin multiplyInteger) A A] A]`,
 * A an 8,000-byte integer: a 16,000 by 8,000-byte product, past the direct
 * carriage bound. */
export const PRODUCT_SCRIPT = failing(
  app(
    lam(call(MULTIPLY, call(MULTIPLY, v(0), v(0)), v(0))),
    int(wide(8_000, 3n)),
  ),
);

/** STEP: `[(builtin modInteger) [(builtin multiplyInteger) A A] (0 - (A +
 * 1))]`, A the 8,000-byte integer of PRODUCT_SCRIPT: a 16,000-byte dividend
 * by a negative 8,000-byte divisor, whose result, 1 - (A + 1), is negative,
 * so the loop steps the remainder against the dividend with the signs
 * reversed and carries one into the claimed result's limbs. */
export const DIVISION_SCRIPT = failing(
  app(
    lam(
      call(
        MOD,
        call(MULTIPLY, v(0), v(0)),
        call(SUBTRACT, int(0n), call(ADD, v(0), int(1n))),
      ),
    ),
    int(wide(8_000, 3n)),
  ),
);

/** `2^n` for `n = 2^k - 1`, built at run time from 2 by `k - 1` rounds of
 * `x -> 2 * x * x`. */
const powerOfTwo = (rounds: number): FlatTerm =>
  Array.from({ length: rounds }).reduce<FlatTerm>(
    (built) =>
      app(lam(call(MULTIPLY, int(2n), call(MULTIPLY, v(0), v(0)))), built),
    int(2n),
  );

/** STEP: `[(builtin multiplyInteger) T W]`, T the largest integer
 * argument, 2^262143 - 1, built at run time as `(h - 1) + h` from h =
 * (2^131071)^2, and W the 8,192-byte 2^65535: the largest staged operand
 * by an 8,192-byte one (midgard-validation
 * cek-executor.staged-product-route.test.ts). */
export const PRODUCT_MAX_SCRIPT = failing(
  call(
    MULTIPLY,
    app(
      lam(call(ADD, call(SUBTRACT, v(0), int(1n)), v(0))),
      app(lam(call(MULTIPLY, v(0), v(0))), powerOfTwo(16)),
    ),
    int(1n << 65_535n),
  ),
);

/** STEP: `[(builtin serialiseData) [(builtin bData) B]]`, B the 16,384
 * bytes of the 64-byte pattern built at run time. */
export const SERIALISE_SCRIPT = failing(
  call(SERIALISE, call(B_DATA, builtBytes(16_384))),
);

/** STEP: `[(builtin serialiseData) [(builtin bData) B]]`, B `length` bytes
 * of the 64-byte pattern built at run time. */
export const serialiseScript = (length: number): string =>
  failing(call(SERIALISE, call(B_DATA, builtBytes(length))));

/** STEP: `[(builtin serialiseData) [(builtin listData) L]]`, L `copies`
 * copies of `[(builtin bData) B]`, B `length` bytes of the 64-byte pattern
 * built once at run time. */
export const serialiseListScript = (copies: number, length: number): string =>
  failing(
    app(
      lam(
        call(
          SERIALISE,
          call(
            LIST_DATA,
            Array.from({ length: copies }).reduce<FlatTerm>(
              (tail) => app(force(builtin(MK_CONS)), call(B_DATA, v(0)), tail),
              { tag: 4, type: [5, 8], value: [] },
            ),
          ),
        ),
      ),
      builtBytes(length),
    ),
  );

/** STEP: `[(builtin byteStringToInteger) True B]`, B the 16,384 bytes of
 * the 64-byte pattern built at run time. */
export const CONVERSION_SCRIPT = failing(
  call(BYTES_TO_INTEGER, bool(true), builtBytes(16_384)),
);

/** STEP: `[(builtin byteStringToInteger) True B]`, B `length` bytes of the
 * 64-byte pattern built at run time. */
export const conversionScript = (length: number): string =>
  failing(call(BYTES_TO_INTEGER, bool(true), builtBytes(length)));

const WRITE_INPUT = bytes(Buffer.alloc(8, 0x5a));

/** STEP: `[(builtin writeBits) I L True]`, I eight bytes and L a list
 * constant of 200 indices, past the 64 the direct route takes. */
export const WRITE_BITS_SCRIPT = failing(
  call(
    WRITE_BITS,
    WRITE_INPUT,
    ints(Array.from({ length: 200 }, (_, index) => BigInt((index * 37) % 64))),
    bool(true),
  ),
);

/** The strict fixpoint `\f. (\x. f (\y. x x y)) (\x. f (\y. x x y))`. */
const fix = (body: FlatTerm): FlatTerm => {
  const half = lam(app(v(1), lam(app(v(1), v(1), v(0)))));
  return app(lam(app(half, half)), body);
};

/** `loop n x = n == 0 ? x : loop (n - 1) step`, applied to `n` and
 * `initial`; `step` reads x as variable 0, n as 1 and the loop as 2. */
const countdown = (n: number, initial: FlatTerm, step: FlatTerm): FlatTerm =>
  app(
    fix(
      lam(
        lam(
          lam(
            force(
              app(
                force(builtin(IF_THEN_ELSE)),
                call(EQUALS_INTEGER, v(1), int(0n)),
                { tag: 1, delayedTerm: v(0) },
                {
                  tag: 1,
                  delayedTerm: app(v(2), call(SUBTRACT, v(1), int(1n)), step),
                },
              ),
            ),
          ),
        ),
      ),
    ),
    int(BigInt(n)),
    initial,
  );

/** STEP: `[(builtin writeBits) I L True]`, I eight bytes and L `constant`
 * indices in a list constant with `rounds` rounds of 64 prepended at run
 * time (as midgard-validation cek-executor.write-bits-loop.max-shape.test.ts
 * builds it). */
export const writeBitsBuiltScript = (
  constant: number,
  rounds: number,
): string =>
  failing(
    call(
      WRITE_BITS,
      WRITE_INPUT,
      countdown(
        rounds,
        ints(
          Array.from({ length: constant }, (_, index) =>
            BigInt((index * 37) % 64),
          ),
        ),
        Array.from({ length: 64 }, (_, index) =>
          BigInt(index),
        ).reduce<FlatTerm>(
          (tail, index) => app(force(builtin(MK_CONS)), int(index), tail),
          v(0),
        ),
      ),
      bool(true),
    ),
  );

/** writeBits at the longest index list a transaction builds: 28,761
 * indices in a list constant, the most that leave the script witness field
 * (the script in its envelope) within its 32,768 bytes, and 442 rounds of 64
 * prepended at run time, the most the transaction budget pays for (443 go
 * past its memory), 57,049 in all, over eight bytes. */
export const WRITE_BITS_MAX_SCRIPT = writeBitsBuiltScript(28_761, 442);
