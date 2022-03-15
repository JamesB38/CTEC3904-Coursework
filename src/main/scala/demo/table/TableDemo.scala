package demo.table

import lib.SQLike._

object TableDemo {

  val city: Table = Table("Name", "Country", "Population")
    .addRow("Leicester", "UK", "500000")
    .addRow("Chennai", "India", "10500000")
    .addRow("Krakow", "Poland", "770000")
    .addRow("Cairo", "Egypt", "20000000")
    .addRow("Beijing", "China", "22000000")
    .addRow("New York", "USA", "9000000")
    .addRow("Moscow", "Russia", "13000000")
    .addRow("Lagos", "Nigeria", "8000000")
    .addRow("Paris", "France", "2000000")
    .addRow("London", "UK", "9000000")
    .addRow("Madrid", "Spain", "3000000")
    .addRow("Osaka", "Japan", "2800000")
    .addRow("Xi'an", "China", "9000000")
    .addRow("Hyderabad", "India", "7000000")
    .addRow("Abuja", "Nigeria", "6000000")
    .addRow("Warsaw", "Poland", "1800000")
    .addRow("Lima", "Peru", "9000000")
    .addRow("Rio de Janeiro", "Brazil", "6500000")
    .addRow("Istanbul", "Turkiye", "15500000")
    .addRow("Nairobi", "Kenya", "4400000")
    .addRow("Riyadh", "Saudi Arabia", "7700000")
    .addRow("Karachi", "Pakistan", "15000000")
    .addRow("Berlin", "Germany", "3800000")
    .addRow("Tokyo", "Japan", "13500000")
    .addRow("Jeddah", "Saudi Arabia", "4700000")
    .addRow("Los Angeles", "USA", "4000000")
    .addRow("Lahore", "Pakistan", "11000000")
    .addRow("Jakarta", "Indonesia", "10000000")
    .addRow("Manchester", "UK", "550000")

  val country: Table = Table("Name", "Continent", "Capital")
    .addRow("Japan", "Asia", "Tokyo")
    .addRow("UK", "Europe", "London")
    .addRow("Egypt", "Africa", "Cairo")
    .addRow("China", "Asia", "Beijing")
    .addRow("Turkiye", "Eurasia", "Ankara")
    .addRow("Poland", "Europe", "Warsaw")
    .addRow("Saudi Arabia", "Asia", "Riyadh")
    .addRow("Brazil", "South America", "Rio de Janeiro")
    .addRow("Kenya", "Africa", "Nairobi")
    .addRow("Norway", "Europe", "Oslo")
    .addRow("Nigeria", "Africa", "Abuja")
    .addRow("India", "Asia", "New Delhi")
    .addRow("France", "Europe", "Paris")
    .addRow("Spain", "Europe", "Madrid")
    .addRow("Pakistan", "Asia", "Islamabad")
    .addRow("USA", "North America", "Washington DC")
    .addRow("Canada", "North America", "Ottowa")
    .addRow("Germany", "Europe", "Berlin")
    .addRow("Indonesia", "Asia", "Jakarta")
    .addRow("Peru", "South America", "Lima")
    .addRow("Russia", "Eurasia", "Moscow")


  def main(args: Array[String]): Unit = {

    //Task 1 a

//    println(city.selectColumns("Name").except(country.selectColumns("Capital")).sortBy("Name"))

    //Task 1 b

    println(country.join(city, "Capital", "Name").selectRows(r => r("R.Population").toInt > 10000000). selectColumns("L.Name", "L.Capital").sortBy("L.Name"))

    //Task 1 c

    //println(city.join(country, "Country", "Name").selectColumns("L.Name", "R.Continent").selectRows(r => r("R.Continent") == "Europe" || r("R.Continent") == "Eurasia" || r("R.Continent") == "Africa").sortBy("L.Name", (v1: Value, v2: Value) => v1 >= v2))

    //List every city with no repeats sorted in reverse alphabetical order of country and sort each city within each country in alphabetical order.
    //eg.
    // [z , a]
    // [z , b]
    // [z , c]
    // [y , a]
    // [y , b]

    //Order all capital cities in order of population


    /** *************************************************************************
      * Examples
      * Uncomment each example in turn
      */

    // Print out the city table and the country table

//    println(city)
//    println(country)

    // Print the country table in ascending order of continent
//    println(country.sortBy("Continent"))

    // Print the city table in descending order of population
//    println(city.sortBy("Population", (v1: Value, v2: Value) => v1.toInt >= v2.toInt))

    // Print the capital cities in Africa
//    println(country.selectRows(r => r("Continent") == "Africa").selectColumns("Capital"))

    // Print the countries contained within the city table, in alphabetical order, without duplicates
//    println(
//      city.selectColumns("Country")
//        .distinct
//        .sortBy("Country")
//    )

    // Print the capital cities in Africa union the cities with population less than 10 million
    // with the results sorted
//    println({
//      val t1 = country.selectRows(r => r("Continent") == "Africa").selectColumns("Capital")
//      val t2 = city.selectRows(r => r("Population").toInt < 10000000).selectColumns("Name")
//      t1.union(t2).sortBy("Capital")
//    })

    // Print in alphabetical order the capital cities in Asia or Eurasia together with their
    // populations
//    println(
//      city.join(country, "Name", "Capital")
//        .selectRows(r => r("R.Continent").endsWith("sia"))
//        .selectColumns("L.Name", "L.Population")
//        .sortBy("L.Name")
//    )

  }
}
