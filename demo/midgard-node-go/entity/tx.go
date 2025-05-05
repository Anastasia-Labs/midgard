package entity

type Transaction struct {
	ID   string `json:"id"`
	Cbor string `json:"cbor" binding:"required"`
}
