package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row, SparkSession}
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import timeusage.TimeUsage.read

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  val timeUsage = TimeUsage

  val (columns, initDf) = read("/timeusage/atussum.csv")
  val (primaryNeedsColumns, workColumns, otherColumns) = timeUsage.classifiedColumns(columns)
  val summaryDf:DataFrame = timeUsage.timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)

  test("classifiedColumns") {
    assert(primaryNeedsColumns.size == 55)
    assert(workColumns.size == 23)
    assert(otherColumns.size == 346)
  }

  test("timeUsageSummary") {
    assert(summaryDf.columns.length === 6)
    assert(summaryDf.count === 114997)
    summaryDf.show()
  }

  test("timeUsageGrouped") {
    val finalDf:DataFrame = timeUsage.timeUsageGrouped(summaryDf)
    val finalTypedDf = timeUsage.timeUsageGrouped(summaryDf)

    finalDf.printSchema()
    finalDf.show(false)

    finalTypedDf.printSchema()
    finalTypedDf.show(false)
  }
}
