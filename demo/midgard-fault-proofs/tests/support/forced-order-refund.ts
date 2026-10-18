import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Constr,
  type Credential,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

/** Rewrites the CBOR of an order datum before the mint pays it. */
export type OrderDatumRewrite = (datumCbor: string) => string;

/**
 * `lucid` whose transaction builders pay any inline datum at `orderAddress`
 * through `rewrite`, as a builder other than the SDK's would write it. The
 * SDK's builder is otherwise unchanged, so the order still carries the SDK's
 * own id, inclusion time, witness and payload.
 */
export const withOrderDatumRewrite = (
  lucid: LucidEvolution,
  orderAddress: string,
  rewrite: OrderDatumRewrite,
): LucidEvolution => {
  const intercept = (builder: object): object => {
    const wrap = (target: object): object =>
      new Proxy(target, {
        get(on, property) {
          const value: unknown = Reflect.get(on, property, on);
          if (typeof value === "function")
            return (...args: unknown[]) => {
              const [address, datum, ...rest] = args;
              const inline = datum as { kind?: unknown; value?: unknown };
              const result: unknown =
                property === "ToAddressWithData" &&
                address === orderAddress &&
                inline.kind === "inline" &&
                typeof inline.value === "string"
                  ? value.apply(on, [
                      address,
                      { ...inline, value: rewrite(inline.value) },
                      ...rest,
                    ])
                  : value.apply(on, args);
              return result === builder ? proxy : result;
            };
          return value !== null && typeof value === "object"
            ? wrap(value)
            : value;
        },
      });
    const proxy = wrap(builder);
    return proxy;
  };
  return new Proxy(lucid, {
    get(on, property) {
      const value: unknown = Reflect.get(on, property, on);
      if (property === "newTx")
        return () => intercept(on.newTx()) as ReturnType<typeof on.newTx>;
      return typeof value === "function" ? value.bind(on) : value;
    },
  });
};

const credential = (data: SDK.CredentialD): Credential =>
  "PublicKeyCredential" in data
    ? { type: "Key", hash: data.PublicKeyCredential[0] }
    : { type: "Script", hash: data.ScriptCredential[0] };

/** The bech32 address an order's `refund_address` names. */
export const orderRefundAddress = (network: Network, order: UTxO): string => {
  const address = Data.from(
    plutusConstrFieldCbor(order.datum!, [3]),
    SDK.AddressData,
  );
  if (address.stakeCredential === null)
    return credentialToAddress(network, credential(address.paymentCredential));
  if (!("Inline" in address.stakeCredential))
    throw new Error("an order refund address has a pointer stake part");
  return credentialToAddress(
    network,
    credential(address.paymentCredential),
    credential(address.stakeCredential.Inline[0]),
  );
};

/**
 * Pay the refund an order's datum names: its refund address, carrying its
 * refund datum. A `DatumHash` refund carries the hash the order holds, and an
 * inline refund datum is paid in the bytes the order holds.
 */
export const payOrderRefund = (
  tx: TxBuilder,
  network: Network,
  order: UTxO,
): TxBuilder => {
  const address = orderRefundAddress(network, order);
  const assets = { lovelace: order.assets.lovelace! };
  const refundDatum = plutusConstrFieldCbor(order.datum!, [4]);
  const { index, fields } = Data.from(refundDatum) as Constr<Data>;
  if (index === 0) return tx.pay.ToAddress(address, assets);
  if (index === 1)
    return tx.pay.ToAddressWithData(
      address,
      { kind: "hash", value: fields[0] as string },
      assets,
    );
  return tx.pay.ToAddressWithData(
    address,
    { kind: "inline", value: plutusConstrFieldCbor(order.datum!, [4, 0]) },
    assets,
  );
};
