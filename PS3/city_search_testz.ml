open City_search
open Assertions

let citi=load_city_data("ithaca.csv")
let world=load_city_data("map.csv")
let world_counter=(((-90.,180.),(90.,180.)),[((-42.,76.),"Cornell")])
let sear=city_search citi ((-90.,-180.),(90.,180.))
let sear1=city_search citi ((42.45,-180.),(90.,180.))
let sear2=city_search citi ((42.442,-76.503),(42.4423,-76.502))
let whole_map=
["lat=42.4417395, long=-76.5118856, name=Oldport Harbour\n";
"lat=42.4422222, long=-76.5022222, name=Greater Ithaca Activities Center\n";
"lat=42.4423826, long=-76.4987232, name=First Presbyterian Church\n";
"lat=42.4438889, long=-76.5016667, name=Calvary Baptist Church\n";
"lat=42.450906, long=-76.4954959, name=Fall Creek School\n";
"lat=42.4339618, long=-76.4924404, name=South Hill School\n";
"lat=42.4367395, long=-76.4766064, name=Belle Sherman School\n"]
let high_map=["lat=42.450906, long=-76.4954959, name=Fall Creek School\n"]
let middle_map=
["lat=42.4422222, long=-76.5022222, name=Greater Ithaca Activities Center\n"]

TEST_UNIT "city_search_test1"=assert_true ((sear)=(whole_map))
TEST_UNIT "city_search_test2"=assert_true ((sear1)=high_map)
TEST_UNIT "city_search_test3"=assert_true (sear2=middle_map)
TEST_UNIT "city_search_test4"=assert_raises (Some (Failure "BadRegion")) (city_search
	world) ((1.,1.),(0.,0.)) 

let () = Pa_ounit_lib.Runtime.summarize ()