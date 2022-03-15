package lib

object SQLike {

  import lib.picture.Picture
  import lib.picture.Picture._

  type Value = String
  type Row = Vector[Value]
  type Name = String

  /**
    * A companion object for class Table which contains a Table constructor
    * (apply) method.
    */
  object Table {
    /**
      * A public constructor for defining a new table.
      *
      * @param attributes The attributes (columns) to be used in the table.
      * @return A new table with no rows of data.
      */
    def apply(attributes: Name*) = new Table(attributes.toVector, Vector.empty)

  } //object Table

  /**
    * A Table is a 2D array of String data. The operations provided allow
    * for the filtering and transformation of data using some basic SQL-style
    * operations. Table instances can be embedded within an application where
    * a small, local, database-like structure is appropriate.
    *
    * Table consistency conditions:
    * 1. All the rows must be the same length.
    * 2. The number of values in a row must equal the number of columns.
    *
    * It is the responsibility of any method that creates a Table to
    * ensure that these consistency conditions are not violated.
    */
  class Table(
               val attr: Vector[Name], // The column headings
               val data: Vector[Row]) // The rows of data
  {
    /**
      * The number of columns is equal to the number of attributes.
      */
    val numberOfColumns: Int = attr.length

    /**
      * The range of valid indexes that can be used to select a column
      * from this table: 0 .. numberOfColumns-1
      */
    val validIndexes: Range = Range(0, numberOfColumns)

    /**
      * A function that maps an attribute name (column header) to the
      * position of that attribute in a row - i.e. the column number.
      */
    val getColumnNumber: Name => Int = s => attr.indexOf(s)


    /**
      * Use the Picture library to create a tabular layout for a Table.
      *
      * @return A Picture representing the table appropriately formatted.
      */
    def toPicture: Picture = {
      val headerRow = attr.map(Picture(_).borderL(' ').borderR(' ')).toList
      val dataRows = data.map(_.map(Picture(_).borderL(' ').borderR(' ')).toList).toList
      makeTableFromRows(headerRow +: dataRows)
    }


    /**
      * Produce a printable version of the Table using the Picture library.
      *
      * @return A nicely formatted table as a String.
      */
    override def toString: Value =
      if (numberOfColumns == 0)
        "No columns"
      else
        this.toPicture.toString

    /**
      * Select specified columns from the current table.
      *
      * @param attributes The sequence of attribute names (columns) required.
      * @return A new table containing only the specified columns from this table.
      */
    def selectColumns(attributes: Name*): Table = {
      // Transform the attribute list into a list of column numbers.
      val selectedColumns: Seq[Int] =
        attributes.map(getColumnNumber).distinct.filter(validIndexes.contains)

      val newAttr: Vector[Name] = selectedColumns.map(attr(_)).toVector

      val newData: Vector[Row] =
        for (row <- data)
          yield selectedColumns.map(row(_)).toVector

      new Table(newAttr, newData)
    }


    /**
      * Select specified rows from the table based upon a condition. Note that
      * because all data values are Strings, then any transformation (e.g. toInt)
      * must be made explicit. (See example below). Furthermore, because the
      * condition function contains attribute names embedded within, these cannot
      * be checked at compile-time. Therefore, if wrong attribute names are used
      * then this will produce a run-time exception. Furthermore, the condition
      * may convert a (String) data value to another format (e.g. toInt). It is
      * the user's responsibility to ensure that any such transformation is valid.
      *
      * @param condition Returns true for any row that satisfies the constraint.
      *                  The user cannot know how the attribute names are mapped
      *                  to columns internally. Therefore, the condition abstracts
      *                  over this function. Using this technique, a user-supplied
      *                  condition can be written in terms of attribute names.
      *                  For example:
      * {{{
      *                  get => get("fruit") == "apple" && get("colour") == "red"
      * }}}
      *                  In this example the parameter to the lambda function (get)
      *                  represents the function that maps attribute names to values
      *                  in a given row. The user is free to choose any name for
      *                  this function, since it is only a parameter. For example,
      *                  we could use "r" so that r("qty") reads "qty on row r":
      * {{{
      *                  r => r("qty").toInt < 10 && r("price").toInt > 100
      * }}}
      * @return A new table containing only the rows from this table that satisfy
      *         the condition.
      */
    def selectRows(condition: (Name => Value) => Boolean): Table = {

      val newData: Vector[Row] =
        data.filter(row => condition(attr => row(getColumnNumber(attr))))

      new Table(attr, newData)
    }


    /**
      * Add a new row to the table. N.B. Adding a duplicate row is not prevented by this
      * operation.
      *
      * @param values The row of data values to be added to the table.
      * @return A mew table like the current one but with the new row added.
      */
    def addRow(values: Value*): Table = {
      val newRow = values.toVector
      if (newRow.size != numberOfColumns)
        throw new Exception(s"addRow: Expected $numberOfColumns values, but got ${newRow.size}")
      new Table(attr, data :+ newRow)
    }

    /**
      * Removes any duplicate rows from the table.
      *
      * @return A new table with any duplicate rows removed.
      */
    def distinct: Table = new Table(attr, data.distinct)

    /**
      * Sort a table into order using a user-supplied order relation and in repsect of
      * a given attribute name (column).
      *
      * @param attribute The column to use for ordering the table.
      * @param inorder   A relation that returns true when first value is in order with
      *                  the second value. If not supplied, then the order relation
      *                  defaults to "<=".
      * @return A new table which is ordered on the given column using the supplied
      *         order relation.
      */
    def sortBy(attribute: Name, inorder: (Value, Value) => Boolean = _ <= _): Table = {
      val selectedColumn: Int = getColumnNumber(attribute)
      if (validIndexes contains selectedColumn) {
        val newData: Vector[Row] =
          data.sortWith((r1, r2) => inorder(r1(selectedColumn), r2(selectedColumn)))
        new Table(attr, newData)
      }
      else
        this
    }


    /**
      * Perform an inner join on two tables. In the joined table the attribute
      * names from this table are prefixed by "L." and the attribute names from
      * that table are prefixed by "R."
      *
      * @param that  The table to be joined.
      * @param attr1 The column in this table to be used in the match.
      * @param attr2 The column in that table to be used in the match.
      * @return A new table representing the inner join of this and that on attr1==attr2.
      */
    def join(that: Table, attr1: Name, attr2: Name): Table = {
      val thisColumnNumber = this.getColumnNumber(attr1)
      val thatColumnNumber = that.getColumnNumber(attr2)
      val newAttr: Vector[Name] = this.attr.map("L." + _) ++ that.attr.map("R." + _)
      val newData: Vector[Row] =
        for (
          thisRow <- this.data;
          thatRow <- that.data
          if thisRow(thisColumnNumber) == thatRow(thatColumnNumber))
        yield thisRow ++ thatRow
      new Table(newAttr, newData)
    }

    /**
      * Performs the union of two tables. The number of columns (attributes) must be the
      * same for this to work. Duplicates are not removed. It is the user's responsibility
      * to ensure that the values added are consistent with the original table.
      * The resulting table keeps the attribute names of the receiver (this) table
      *
      * @param that The table whose rows are to be added to the current table.
      * @return A new table consisting of the rows of this table and that table.
      */
    def union(that: Table): Table = {
      if (this.numberOfColumns != that.numberOfColumns)
        throw new Exception("union: different number of columns")
      new Table(this.attr, this.data ++ that.data)
    }

    /**
      * Performs the intersection of two tables. The number of columns (attributes) must be
      * the same for this to work. It is the user's responsibility to ensure that the values
      * intersected are consistent with the original table.
      * The resulting table keeps the attribute names of the receiver (this) table
      *
      * @param that The table whose rows are to be intersected with the current table.
      * @return A new table consisting of the rows common to this table and that table.
      */
    def intersect(that: Table): Table = {
      if (this.numberOfColumns != that.numberOfColumns)
        throw new Exception("intersect: different number of columns")
      new Table(this.attr, this.data intersect that.data)
    }


    /**
      * Performs the difference of two tables. The number of columns (attributes) must be
      * the same for this to work. It is the user's responsibility to ensure that the values
      * compared are consistent with the original table.
      * The resulting table keeps the attribute names of the receiver (this) table
      *
      * @param that The table whose rows are to be removed from the current table.
      * @return A new table consisting of the rows in this table but not in that table.
      */
    def except(that: Table): Table = {
      if (this.numberOfColumns != that.numberOfColumns)
        throw new Exception("except: different number of columns")
      new Table(this.attr, this.data diff that.data)
    }


    /** *****************************************************************************
      * Coursework questions
      * ***************************************************************************** */

    /**
      * Renames the selected attributes (column headers) in the table. For example:
      * {{{
      *   tab3.renameAttributes("Area Code" -> "Region", "Qty" -> "No in stock")
      * }}}
      *
      * @param changes The list of mappings from old names to new names.
      * @return A new table with the matched attribute names changed according to
      *         the list of changes.
      */
    def renameAttributes(changes: (Name, Name)*): Table = {
      ???
    }


    /**
      * Update a table by transforming the values in a given column on rows that
      * satisfy the given condition
      *
      * @param attribute The name of the column whose values are to be transformed.
      * @param transform Describes how to update each data value in the given column.
      * @param condition Returns true for any row that satisfies the constraint.
      *                  The user cannot know how the attribute names are mapped
      *                  to columns internally. Therefore, the condition abstracts
      *                  over this function. Using this technique, a user-supplied
      *                  condition can be written in terms of attribute names.
      *                  For example:
      * {{{
      *                  get => get("fruit") == "apple" && get("colour") == "red"
      * }}}
      *                  In this example the parameter to the lambda function (get)
      *                  represents the function that maps attribute names to values
      *                  in a given row. The user is free to choose any name for
      *                  this function, since it is only a parameter. For example,
      *                  we could use "r" so that r("qty") reads "qty on row r":
      * {{{
      *                  r => r("qty").toInt < 10 && r("price").toInt > 100
      * }}}
      * @return A new table in which the values in the given column are updated
      *         according to the transform function. If the supplied attribute name is
      *         not in the table then this table is returned unchanged.
      */
    def update(attribute: Name,
               transform: Value => Value,
               condition: (Name => Value) => Boolean): Table = {
      ???
    }


  } //class Table


}
