/**
 * The script-decode step golden rows over Data, values and byte strings in
 * bulk runs: every CBOR item form, items and keys split over Flat chunks,
 * refusals mid-run, chunks past the script end, and the integer and fold
 * limits a tick ends at.
 *
 * This module imports nothing from the package, so either side can load it.
 */

import {
  con,
  pow2,
  program,
  vBytes,
  vChunks,
  vInt,
  vList,
  vPair,
  vUnit,
  vValue,
} from "./script-decode-step-v1.writers.mjs";

/** CBOR items of every form a Data constant may take, as hex. */
const DATA_FORMS = [
  "00",
  "17",
  "1818",
  "190100",
  "1a00010000",
  "1b0000000100000000",
  "1bffffffffffffffff",
  "20",
  "3bffffffffffffffff",
  "1805",
  "190005",
  "c240",
  "c24105",
  "c34100",
  "c25f4101ff",
  `c35820${"ff".repeat(32)}`,
  "40",
  "4401020304",
  `5840${"ab".repeat(64)}`,
  "5f4101420203ff",
  "5fff",
  `5f5840${"01".repeat(64)}5840${"02".repeat(64)}4103ff`,
  "80",
  "9fff",
  "9f0102ff",
  "820102",
  "9f9f9f01ffffff",
  "a0",
  "a10102",
  "bf0102ff",
  "bfff",
  "a281800180a0",
  "d87980",
  "d8799f01ff",
  "d87f80",
  "d9050080",
  "d9057880",
  "d866820080",
  "d866821880820102",
  "d8669f18809f01ffff",
  "d866821bffffffffffffffff80",
  "0102",
];

const hexBytes = (text) => Buffer.from(text, "hex");

/** `bytes` as Flat chunks of the given sizes, repeated. */
const split = (bytes, sizes) => {
  const parts = [];
  for (let offset = 0, index = 0; offset < bytes.length; index += 1) {
    const size = sizes[index % sizes.length];
    parts.push(bytes.subarray(offset, offset + size));
    offset += size;
  }
  return parts;
};

/** A Value of `[currency, [[token, quantity]]]` with each key as Flat chunks. */
const vChunkedValue = (entries) =>
  vList(
    entries.map(([currency, tokens]) =>
      vPair(
        vChunks(currency),
        vList(
          tokens.map(([name, quantity]) =>
            vPair(vChunks(name), vInt(quantity)),
          ),
        ),
      ),
    ),
  );

/**
 * Constant lists of Data, values and byte strings over several Flat chunks,
 * the elements the bulk path decodes past one chunk, and the elements it
 * leaves to the one-node ticks.
 */
export const bulkDataPrograms = () => {
  const range = (n) => Array.from({ length: n }, (_, index) => index);
  const forms = DATA_FORMS.map(hexBytes);
  const longList = Buffer.concat([
    Buffer.from([0x9f]),
    Buffer.alloc(100, 1),
    Buffer.from([0xff]),
  ]);
  const zero = (n) => Buffer.alloc(n);
  return [
    [
      "bulk list Data, every item form",
      program(
        0,
        con(
          [7, 5, 8],
          vList(
            [...forms.slice(0, 20), longList, ...forms.slice(20)].map(vBytes),
          ),
        ),
      ),
    ],
    [
      "bulk list Data over Flat chunks",
      program(
        0,
        con(
          [7, 5, 8],
          vList([
            ...forms.map((form, index) =>
              vChunks(split(form, index % 2 === 0 ? [1] : [2, 1])),
            ),
            vChunks([[0x01], [0x02, 0x03]]),
          ]),
        ),
      ),
    ],
    // Runs of 512 units of one cheap item form after another, so every unit
    // an item is charged moves the element a tick ends at.
    [
      "bulk list Data, runs to the tick's units",
      program(
        0,
        con(
          [7, 5, 8],
          vList(
            [
              vChunks([[0x00], [0x01], [0x02]]),
              vBytes(hexBytes("80")),
              vBytes(hexBytes("d87980")),
              vBytes(hexBytes("a10000")),
              vBytes(hexBytes("5f4101ff")),
              vBytes(hexBytes("c24101")),
            ].flatMap((item) => range(80).map(() => item)),
          ),
        ),
      ),
    ],
    [
      "bulk list Data refused mid-list",
      program(
        0,
        con(
          [7, 5, 8],
          vList([...forms.slice(0, 30), hexBytes("1c"), ...forms].map(vBytes)),
        ),
      ),
    ],
    [
      "bulk list Data, a chunk past the script end",
      program(
        0,
        con(
          [7, 5, 8],
          vList([...forms.slice(0, 6).map(vBytes), vChunks([[0x19, 0, 5]])]),
        ),
      ).subarray(0, -4),
    ],
    [
      "bulk chain over lists of Data, empty and not",
      program(
        0,
        con(
          [7, 5, 7, 7, 6, 3, 7, 7, 6, 3, 7, 5, 8],
          vList(
            range(80).map((i) =>
              vPair(
                vUnit(),
                vPair(
                  vUnit(),
                  vList(i % 2 === 0 ? [] : [vBytes(forms[i % forms.length])]),
                ),
              ),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk nested lists of Data",
      program(
        0,
        con(
          [7, 5, 7, 5, 8],
          vList(
            range(60).map((i) =>
              vList(
                range(i % 3).map((j) => vBytes(forms[(i + j) % forms.length])),
              ),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list value",
      program(
        0,
        con(
          [7, 5, 13],
          vList([
            ...range(30).map((i) =>
              vValue([
                ["", [["", BigInt(i + 1)]]],
                [
                  "0a".repeat(28),
                  [
                    ["01", -pow2(127)],
                    ["0102", pow2(127) - 1n],
                    ["ff".repeat(32), BigInt(-i - 1)],
                  ],
                ],
              ]),
            ),
            vChunkedValue([
              [
                [[1], [2, 3]],
                [
                  [[[4]], 5n],
                  [[[4], [0]], -6n],
                ],
              ],
              [[[1, 2, 4]], [[[], 7n]]],
            ]),
            vValue(
              range(40).map((i) => [`${(i + 16).toString(16)}`, [["", 1n]]]),
            ),
          ]),
        ),
      ),
    ],
    [
      "bulk list value refused mid-list",
      program(
        0,
        con(
          [7, 5, 13],
          vList([
            ...range(10).map((i) => vValue([["", [["", BigInt(i + 1)]]]])),
            vValue([
              [
                "01",
                [
                  ["02", 1n],
                  ["01", 1n],
                ],
              ],
            ]),
            vValue([["", [["", 1n]]]]),
          ]),
        ),
      ),
    ],
    [
      "bulk list value, a quantity of 2^127",
      program(
        0,
        con(
          [7, 5, 13],
          vList([
            ...range(10).map((i) => vValue([["", [["", BigInt(-i - 1)]]]])),
            vValue([["", [["", pow2(127)]]]]),
          ]),
        ),
      ),
    ],
    [
      "bulk list bytestring over Flat chunks",
      program(
        0,
        con(
          [7, 5, 1],
          vList([
            ...range(40).map((i) =>
              vChunks(split(Buffer.alloc(i + 2, i), [1, 2])),
            ),
            vChunks([Buffer.alloc(255, 1), Buffer.alloc(255, 2), [3]]),
            vChunks(split(zero(4095), [255])),
            vChunks(split(zero(4096), [255])),
          ]),
        ),
      ),
    ],
    [
      "bulk list string over Flat chunks",
      program(
        0,
        con(
          [7, 5, 2],
          vList(
            range(40).map((i) =>
              vChunks(
                i % 2 === 0
                  ? [[0xe2], [0x82, 0xac], [0x61 + (i % 20)]]
                  : [[0x61], [0x62]],
              ),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list string refused over Flat chunks",
      program(
        0,
        con(
          [7, 5, 2],
          vList([
            ...range(20).map(() => vChunks([[0xe2, 0x82], [0xac]])),
            vChunks([[0xe2, 0x82], [0x41]]),
          ]),
        ),
      ),
    ],
    [
      "bulk list bytestring, a last chunk past the script end",
      program(
        0,
        con(
          [7, 5, 1],
          vList([[0xaa], [0xbb], [0xcc], [0xdd, 0xee]].map(vBytes)),
        ),
      ).subarray(0, -3),
    ],
    [
      "bulk list string, a last chunk past the script end",
      program(
        0,
        con([7, 5, 2], vList([[0x61], [0x62], [0x63, 0x64]].map(vBytes))),
      ).subarray(0, -3),
    ],
    [
      "bulk list integer at the 20-group limit",
      program(
        0,
        con(
          [7, 5, 0],
          vList(
            range(70).map((i) =>
              vInt(i % 2 === 0 ? pow2(139) - 1n : -pow2(139)),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list integer past the 20-group limit",
      program(
        0,
        con(
          [7, 5, 0],
          vList(
            range(65).map((i) =>
              vInt(i % 2 === 0 ? pow2(139) : -pow2(139) - 2n),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk fold of 64 items",
      program(0, con([7, 5, 0], vList(range(64).map(() => vInt(pow2(150)))))),
    ],
    // 64 items fill a fold tick. The first fold tick of 130 items starts a
    // 3-tick Walk, so that Walk reveals all 130 frames and folds 64, 64 and 2.
    [
      "bulk fold of 130 items",
      program(0, con([7, 5, 0], vList(range(130).map(() => vInt(pow2(150)))))),
    ],
  ];
};
