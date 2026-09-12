import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";

export const policy = (byte = "11") => `82581c${byte.repeat(28)}`;
export const mintItem = (assets = "a14001", byte = "11") =>
  Buffer.from(policy(byte) + assets, "hex");
export const malformedMintItems = [
  ["ADA policy", "8240a14005"],
  ["short policy", `82581b${"11".repeat(27)}a14001`],
  ["long policy", `82581d${"11".repeat(29)}a14001`],
  ["wrong tuple", `83${policy().slice(2)}a14001`],
  ["nonminimal tuple", `9802${policy().slice(2)}a14001`],
  ["nonminimal policy bytes", `8259001c${"11".repeat(28)}a14001`],
  ["empty map", policy() + "a0"],
  ["indefinite map", policy() + "bf4001ff"],
  ["nonminimal map", policy() + "b8014001"],
  ["oversize name", policy() + `a15821${"11".repeat(33)}01`],
  ["zero quantity", policy() + "a14000"],
  ["nonminimal positive", policy() + "a1401801"],
  ["nonminimal negative", policy() + "a1403800"],
  ["tagged integer", policy() + "a140c24101"],
  ["duplicate asset", policy() + "a240014002"],
  ["descending asset", policy() + "a2410201410101"],
  ["trailing bytes", policy() + "a1400100"],
  ["truncated integer", policy() + "a1401b01"],
  ["truncated policy", "82581c11"],
  ["wrong major type", "00"],
] as const;
export const canonicalMintItems = [
  mintItem(),
  mintItem("a14020"),
  mintItem("a1401bffffffffffffffff"),
  mintItem("a1403bffffffffffffffff"),
  mintItem("a3400141002042010002"),
];
/** Exactly 32764 item bytes + the four-byte field wrapper = 32768. */
export const maximumCanonicalMintItem = () => {
  const entries = [
    Buffer.concat([Buffer.from([0x43]), Buffer.alloc(3), Buffer.from([1])]),
  ];
  for (let i = 0; i < 935; i += 1) {
    const name = Buffer.alloc(32);
    name.writeUInt16BE(i, 30);
    entries.push(
      Buffer.concat([Buffer.from([0x58, 32]), name, Buffer.from([1])]),
    );
  }
  return Buffer.concat([Buffer.from(policy() + "b903a8", "hex"), ...entries]);
};
export const mintField = (...items: Buffer[]) =>
  encodeMidgardFieldPreimage(items);

/** Near the field byte bound, with a fault after 909 valid policy groups. */
export const lateMalformedMintItems = () =>
  Array.from({ length: 910 }, (_, index) => {
    const key = Buffer.alloc(28);
    key.writeUInt16BE(index, 26);
    return Buffer.concat([
      Buffer.from("82581c", "hex"),
      key,
      Buffer.from(index === 909 ? "a14000" : "a14001", "hex"),
    ]);
  });
