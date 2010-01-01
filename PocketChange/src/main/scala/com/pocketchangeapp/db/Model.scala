package com.pocketchangeapp.db

import com.osinka.mongodb._

trait Model[T] { self: ObjectShape[T] =>
    type FieldIndex = (ObjectField[_], SortOrder)

    def indexes: List[Seq[FieldIndex]]
    def coll: ShapedCollection[T]
}
