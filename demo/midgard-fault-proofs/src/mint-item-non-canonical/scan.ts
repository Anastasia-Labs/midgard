/** Total, bounded §5.6 byte scanner; mirrors the on-chain scan module. */
export type MintItemScanControl = Readonly<{
  stage: number;
  cursor: number;
  remaining: number;
  previousPolicy: string;
  previousAsset: string | null;
}>;
export const initialMintItemScanControl = (
  previousPolicy = "",
): MintItemScanControl => ({
  stage: 0,
  cursor: 0,
  remaining: 0,
  previousPolicy,
  previousAsset: null,
});
type Head = { offset: number; value: bigint };
const head = (bytes: Buffer, offset: number, major: number): Head | null => {
  const first = bytes[offset];
  if (first === undefined || first >>> 5 !== major) return null;
  const additional = first & 31;
  if (additional < 24) return { offset: offset + 1, value: BigInt(additional) };
  if (additional > 27) return null;
  const length = 2 ** (additional - 24);
  if (offset + 1 + length > bytes.length) return null;
  let value = 0n;
  for (let i = 0; i < length; i += 1)
    value = value * 256n + BigInt(bytes[offset + 1 + i]!);
  const minimum = [24n, 256n, 65_536n, 4_294_967_296n][additional - 24]!;
  return value < minimum ? null : { offset: offset + 1 + length, value };
};
export const mintItemPolicyId = (item: Buffer): string | null =>
  item.length >= 31 && item.subarray(0, 3).toString("hex") === "82581c"
    ? item.subarray(3, 31).toString("hex")
    : null;
const precedes = (left: string, right: string) =>
  left.length < right.length || (left.length === right.length && left < right);
export const advanceMintItemScan = (
  control: MintItemScanControl,
  total: number,
  window: Buffer,
  offset: number,
): MintItemScanControl | null => {
  if (control.stage === 0) {
    const policy = mintItemPolicyId(window);
    const map = head(window, 31, 5);
    if (
      policy === null ||
      map === null ||
      map.value === 0n ||
      map.value > 0xffff_ffffn ||
      map.offset >= total ||
      (control.previousPolicy !== "" &&
        !precedes(control.previousPolicy, policy))
    )
      return null;
    return {
      ...control,
      stage: 1,
      cursor: map.offset,
      remaining: Number(map.value),
    };
  }
  let current = control;
  let localOffset = offset;
  for (let budget = 32; budget > 0; budget -= 1) {
    const nameHead = head(window, localOffset, 2);
    if (nameHead === null || nameHead.value > 32n) return null;
    const nameEnd = nameHead.offset + Number(nameHead.value);
    if (nameEnd > window.length) return null;
    const name = window.subarray(nameHead.offset, nameEnd).toString("hex");
    const positive = head(window, nameEnd, 0);
    const quantity =
      positive === null
        ? head(window, nameEnd, 1)
        : positive.value === 0n
          ? null
          : positive;
    if (
      quantity === null ||
      (current.previousAsset !== null && !precedes(current.previousAsset, name))
    )
      return null;
    const cursor = current.cursor + quantity.offset - localOffset;
    const remaining = current.remaining - 1;
    if (remaining === 0 ? cursor !== total : cursor >= total) return null;
    current = {
      ...current,
      cursor,
      remaining,
      previousAsset: name,
      stage: remaining === 0 ? 2 : 1,
    };
    localOffset = quantity.offset;
    if (current.stage === 2 || localOffset >= 4_095) break;
  }
  return current;
};
export const mintItemScanControlData = (control: MintItemScanControl) => ({
  stage: BigInt(control.stage),
  cursor: BigInt(control.cursor),
  remaining: BigInt(control.remaining),
  previous_policy: control.previousPolicy,
  previous_asset: control.previousAsset,
});
