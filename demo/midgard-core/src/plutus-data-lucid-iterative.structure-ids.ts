/**
 * Structural identity for Plutus Data nodes, as CML compares `PlutusData`:
 * integers by value, byte strings by content, lists and constructor fields
 * item by item, maps entry by entry in order, constructors by alternative, and
 * encoding details ignored.
 *
 * Nodes are numbered by the caller and registered children first. Each node
 * gets a small integer id, equal exactly when the nodes are structurally
 * equal; a node's signature names only its children's ids, so the whole
 * numbering is linear in the size of the tree.
 */
export class PlutusDataStructureIds {
  private readonly ids: number[];
  private readonly interned = new Map<string, number>();

  constructor(size: number) {
    this.ids = new Array<number>(size);
  }

  of(node: number): number {
    const id = this.ids[node];
    if (id === undefined) {
      throw new Error("Plutus Data structure id is not assigned yet");
    }
    return id;
  }

  integer(node: number, value: bigint): void {
    this.assign(node, `i${value.toString()}`);
  }

  bytes(node: number, hex: string): void {
    this.assign(node, `b${hex}`);
  }

  list(node: number, children: readonly number[]): void {
    this.assign(node, `l${this.join(children)}`);
  }

  map(node: number, children: readonly number[]): void {
    this.assign(node, `m${this.join(children)}`);
  }

  constr(node: number, alternative: bigint, children: readonly number[]): void {
    this.assign(node, `c${alternative.toString()}:${this.join(children)}`);
  }

  private join(children: readonly number[]): string {
    return children.map((child) => this.of(child).toString()).join(",");
  }

  private assign(node: number, signature: string): void {
    let id = this.interned.get(signature);
    if (id === undefined) {
      id = this.interned.size;
      this.interned.set(signature, id);
    }
    this.ids[node] = id;
  }
}
