package com.pocketchangeapp.db

import com.osinka.mongodb._

trait Model[T] { shape: ObjectShape[T] =>
    type FieldIndex = (ObjectField[_], SortOrder)

    val collectionName: String

    def indexes: List[Seq[FieldIndex]]

    def getCollection = shape.collection(Database getCollection collectionName)
}