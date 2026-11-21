import { computeHash32 } from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

export type StructuredDataTree =
  | { kind: "reference"; index: number }
  | { kind: "constructor"; tag: number; fields: StructuredDataTree }
  | {
      kind: "list" | "bytes_parts" | "list_parts" | "map_parts";
      items: StructuredDataTree[];
    }
  | { kind: "map"; items: [StructuredDataTree, StructuredDataTree][] };
const encode = (data: Data) =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(Data.to(data));

/** Publish already decoded Data, so consumers never reparse large asset maps. */
export const structuredDataPublicationPlan = (preimageHex: string) => {
  const root = Data.from(preimageHex);
  if (encode(root) !== preimageHex || preimageHex.length > 65536 * 2)
    throw new Error("structured evidence is not bounded typed Data");
  const publicationDatums: string[] = [];
  const visit = (value: Data): StructuredDataTree => {
    const datum = Data.to(value);
    if (datum.length <= 14000 * 2) {
      const present = publicationDatums.indexOf(datum);
      const index = present < 0 ? publicationDatums.length : present;
      if (present < 0) publicationDatums.push(datum);
      return { kind: "reference", index };
    }
    if (value instanceof Constr)
      return {
        kind: "constructor",
        tag: value.index,
        fields: visit(value.fields),
      };
    if (typeof value === "string") {
      const items = [];
      for (let offset = 0; offset < value.length; offset += 13000 * 2)
        items.push(visit(value.slice(offset, offset + 13000 * 2)));
      return { kind: "bytes_parts", items };
    }
    if (Array.isArray(value)) {
      const groups: Data[][] = [];
      let group: Data[] = [];
      let size = 2;
      for (const item of value) {
        const next = Data.to(item).length / 2;
        if (group.length > 0 && size + next > 10000) {
          groups.push(group);
          group = [];
          size = 2;
        }
        group.push(item);
        size += next;
      }
      if (group.length > 0) groups.push(group);
      return {
        kind: "list_parts",
        items: groups.map((items) =>
          items.length === 1 && Data.to(items).length > 28000
            ? { kind: "list", items: items.map(visit) }
            : visit(items),
        ),
      };
    }
    if (value instanceof Map) {
      const groups: Map<Data, Data>[] = [];
      let group = new Map<Data, Data>();
      let size = 2;
      for (const [key, item] of value) {
        const next = (Data.to(key).length + Data.to(item).length) / 2;
        if (group.size > 0 && size + next > 10000) {
          groups.push(group);
          group = new Map();
          size = 2;
        }
        group.set(key, item);
        size += next;
      }
      if (group.size > 0) groups.push(group);
      return {
        kind: "map_parts",
        items: groups.map((items) =>
          items.size === 1 && Data.to(items).length > 28000
            ? {
                kind: "map",
                items: [...items].map(([key, item]) => [
                  visit(key),
                  visit(item),
                ]),
              }
            : visit(items),
        ),
      };
    }
    throw new Error("an atomic integer exceeds one publishable datum");
  };
  const tree = visit(root);
  if (publicationDatums.length > 64)
    throw new Error(
      "structured evidence exceeded its bounded publication frontier",
    );
  const publications = publicationDatums.map((datum, chunkIndex) => {
    const bytes = Buffer.from(datum, "hex");
    return { chunkIndex, bytes, digest: computeHash32(bytes) };
  });
  return {
    tree,
    plan: { tier: "RawDatums" as const, publications },
    publicationDatums,
    publicationDigests: publications.map((p) => p.digest.toString("hex")),
  };
};

export const structuredDataTreeData = (
  tree: StructuredDataTree,
  referenceIndex: (index: number) => bigint,
): Data => {
  const recur = (node: StructuredDataTree): Data =>
    structuredDataTreeData(node, referenceIndex);
  switch (tree.kind) {
    case "reference":
      return new Constr(0, [referenceIndex(tree.index)]);
    case "constructor":
      return new Constr(1, [BigInt(tree.tag), recur(tree.fields)]);
    case "list":
      return new Constr(2, [tree.items.map(recur)]);
    case "map":
      return new Constr(3, [
        new Map(tree.items.map(([key, value]) => [recur(key), recur(value)])),
      ]);
    case "bytes_parts":
      return new Constr(4, [tree.items.map(recur)]);
    case "list_parts":
      return new Constr(5, [tree.items.map(recur)]);
    case "map_parts":
      return new Constr(6, [tree.items.map(recur)]);
  }
};
