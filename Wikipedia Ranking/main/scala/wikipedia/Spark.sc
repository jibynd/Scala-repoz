import org.apache.spark._

val conf: SparkConf = new SparkConf().setMaster("local[4]").setAppName("MyWiki")
val sc: SparkContext = new SparkContext(conf)

sc.parallelize(List(1,2,3)) collect()
