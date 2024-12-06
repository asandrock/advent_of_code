program main
  use StringiFor
  use PENF, only: I8P
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  integer :: funit
  type(string), allocatable :: lines(:), parts(:)

  integer(I8P), allocatable :: seeds(:)
  integer(I8P), allocatable, dimension(:,:) :: seed_to_soil, soil_to_fertilizer, &
    fertilizer_to_water, water_to_light, light_to_temperature, &
    temperature_to_humidity, humidity_to_location
  integer(I8P) :: j, k, old_empty(1), empty(1), seed, location, min_location

  open(newunit=funit, file=filename)
  call read_lines(funit, lines)
  close(funit)

  ! List of seeds
  call lines(1)%split(tokens=parts, sep=' ')
  allocate(seeds(size(parts) - 1))
  do j = 1, size(parts) - 1
    !seeds(j) = parts(j + 1)%to_number(kind=1_I1P)
    read (parts(j + 1)%raw, *) seeds(j)
  end do

  ! Maps
  ! Seed-to-soil map
  old_empty = 2
  empty = minloc(lines(old_empty(1) + 1:)%len())
  allocate(seed_to_soil(old_empty(1) + 1, empty(1) - 2))
  do j = 1, empty(1) - 2
    read(lines(old_empty(1) + 1 + j)%raw, *) seed_to_soil(1, j), &
      seed_to_soil(2, j), seed_to_soil(3, j)
  end do
  !print *, seed_to_soil
  old_empty = old_empty + empty
  ! Soil-to-fertilizer map
  empty = minloc(lines(old_empty(1) + 1:)%len())
  !print *, empty
  allocate(soil_to_fertilizer(old_empty(1) + 1, empty(1) - 2))
  do j = 1, empty(1) - 2
    read(lines(old_empty(1) + 1 + j)%raw, *) soil_to_fertilizer(1, j), &
      soil_to_fertilizer(2, j), soil_to_fertilizer(3, j)
  end do
  !print *, soil_to_fertilizer
  old_empty = old_empty + empty
  ! Fertilizer-to-water map
  empty = minloc(lines(old_empty(1) + 1:)%len())
  allocate(fertilizer_to_water(old_empty(1) + 1, empty(1) - 2))
  do j = 1, empty(1) - 2
    read(lines(old_empty(1) + 1 + j)%raw, *) fertilizer_to_water(1, j), &
      fertilizer_to_water(2, j), fertilizer_to_water(3, j)
  end do
  !print *, fertilizer_to_water
  old_empty = old_empty + empty
  ! Water-to-light map
  empty = minloc(lines(old_empty(1) + 1:)%len())
  allocate(water_to_light(old_empty(1) + 1, empty(1) - 2))
  do j = 1, empty(1) - 2
    read(lines(old_empty(1) + 1 + j)%raw, *) water_to_light(1, j), &
      water_to_light(2, j), water_to_light(3, j)
  end do
  !print *, water_to_light
  old_empty = old_empty + empty
  ! Light-to-temperature map
  empty = minloc(lines(old_empty(1) + 1:)%len())
  allocate(light_to_temperature(old_empty(1) + 1, empty(1) - 2))
  do j = 1, empty(1) - 2
    read(lines(old_empty(1) + 1 + j)%raw, *) light_to_temperature(1, j), &
      light_to_temperature(2, j), light_to_temperature(3, j)
  end do
  !print *, light_to_temperature
  old_empty = old_empty + empty
  ! Temperature-to-humidity map
  empty = minloc(lines(old_empty(1) + 1:)%len())
  allocate(temperature_to_humidity(old_empty(1) + 1, empty(1) - 2))
  do j = 1, empty(1) - 2
    read(lines(old_empty(1) + 1 + j)%raw, *) temperature_to_humidity(1, j), &
      temperature_to_humidity(2, j), temperature_to_humidity(3, j)
  end do
  !print *, temperature_to_humidity
  old_empty = old_empty + empty
  ! Humidity-to-location map
  empty = minloc(lines(old_empty(1) + 1:)%len())
  allocate(humidity_to_location(old_empty(1) + 1, empty(1) - 2))
  do j = 1, empty(1) - 2
    read(lines(old_empty(1) + 1 + j)%raw, *) humidity_to_location(1, j), &
      humidity_to_location(2, j), humidity_to_location(3, j)
  end do
  !print *, humidity_to_location
  ! Find out location for each seed
  do j = 1, size(seeds), 2
    do k = 0, seeds(j + 1)
      seed = seeds(j) + k
      location = mapping(humidity_to_location, &
        mapping(temperature_to_humidity, &
        mapping(light_to_temperature, &
        mapping(water_to_light, &
        mapping(fertilizer_to_water, &
        mapping(soil_to_fertilizer, &
        mapping(seed_to_soil, seed)))))))
      if (j == 1 .and. k == 0) then
        min_location = location
      else
        min_location = min(location, min_location)
      end if
    end do
  end do
  print *, min_location
contains
  function mapping(map, input)
    implicit none
    integer(I8P), intent(in) :: map(:,:), input
    integer(I8P) :: mapping

    integer(I8P) :: k, from, to, num

    do k = 1, size(map, dim=2)
      from = map(2, k)
      to = map(1, k)
      num = map(3, k)
      if (input >= from .and. input <= from + num) then
        mapping = to + (input - from)
        return
      end if
    end do
    mapping = input
  end function mapping
end program main
