declare module "blake2b" {
  type Blake2bHash = {
    readonly update: (input: Uint8Array) => Blake2bHash;
    readonly digest: () => Uint8Array;
  };

  const blake2b: (outputLength: number) => Blake2bHash;
  export default blake2b;
}
